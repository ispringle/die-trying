(defpackage die-trying-tests
  (:use :cl :fiveam :die-trying)
  (:export :run-tests))

(in-package #:die-trying-tests)

;;; Test suite definition
(def-suite die-trying-tests
  :description "Test suite for die-trying static site generator.")

(in-suite die-trying-tests)

;;; Test fixture helpers

(defun make-temp-directory ()
  "Create a temporary directory for testing."
  (let ((temp-dir (format nil "/tmp/die-trying-test-~a/" (get-universal-time))))
    (ensure-directories-exist temp-dir)
    temp-dir))

(defun delete-temp-directory (dir)
  "Recursively delete a temporary directory."
  (when (probe-file dir)
    (cl-fad:delete-directory-and-files dir)))

(defmacro with-temp-directory ((var) &body body)
  "Execute body with a temporary directory bound to var, cleaning up afterwards."
  `(let ((,var (make-temp-directory)))
     (unwind-protect
          (progn ,@body)
       (delete-temp-directory ,var))))

(defun create-test-file (path content)
  "Create a test file with the given content."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string content stream)))

(defun read-file-content (path)
  "Read entire file content as string."
  (with-open-file (stream path :direction :input)
    (let ((seq (make-string (file-length stream))))
      (read-sequence seq stream)
      seq)))

(defun file-exists-p (path)
  "Check if file exists."
  (probe-file path))

;;; Utility function tests

(test vector-to-list-test
  "Test conversion of vector to list"
  (is (equal '(1 2 3) (vector-to-list #(1 2 3))))
  (is (equal '() (vector-to-list #())))
  (is (equal '("a" "b" "c") (vector-to-list #("a" "b" "c")))))

(test vector-empty-p-test
  "Test empty vector detection"
  (is (vector-empty-p #()))
  (is-false (vector-empty-p #(1)))
  (is-false (vector-empty-p #(1 2 3))))

(test topological-sort-test
  "Test topological sorting of fragments by dependencies"
  ;; Test with no dependencies
  (let ((frags (list (list :tag "a" :dom nil :depends-on nil)
                     (list :tag "b" :dom nil :depends-on nil))))
    (let ((sorted (topological-sort frags)))
      (is (= 2 (length sorted)))))
  
  ;; Test with simple dependencies: b depends on a
  (let ((frags (list (list :tag "b" :dom nil :depends-on '("a"))
                     (list :tag "a" :dom nil :depends-on nil))))
    (let ((sorted (topological-sort frags)))
      (is (= 2 (length sorted)))
      ;; 'a' should come before 'b'
      (let ((tags (mapcar (lambda (f) (getf f :tag)) sorted)))
        (is (< (position "a" tags :test #'string=)
               (position "b" tags :test #'string=))))))
  
  ;; Test with multi-level dependencies: c -> b -> a
  (let ((frags (list (list :tag "c" :dom nil :depends-on '("b"))
                     (list :tag "b" :dom nil :depends-on '("a"))
                     (list :tag "a" :dom nil :depends-on nil))))
    (let ((sorted (topological-sort frags)))
      (is (= 3 (length sorted)))
      (let ((tags (mapcar (lambda (f) (getf f :tag)) sorted)))
        (is (< (position "a" tags :test #'string=)
               (position "b" tags :test #'string=)))
        (is (< (position "b" tags :test #'string=)
               (position "c" tags :test #'string=)))))))

(test categorize-file-test
  "Test file categorization"
  (with-temp-directory (temp-dir)
    ;; Test asset file (non-HTML)
    (let* ((css-filename "test.css")
           (css-file (merge-pathnames css-filename temp-dir)))
      (create-test-file css-file "body { color: red; }")
      (is (equal :asset (categorize-file css-file))))
    
    ;; Test fragment (HTML without <html> tag)
    (let* ((fragment-filename "test-fragment.html")
           (fragment-file (merge-pathnames fragment-filename temp-dir)))
      (create-test-file fragment-file "<div>Fragment content</div>")
      (is (equal :fragment (categorize-file fragment-file))))
    
    ;; Test template (HTML with <html> tag)
    (let* ((template-filename "test-template.html")
           (template-file (merge-pathnames template-filename temp-dir)))
      (create-test-file template-file "<!DOCTYPE html><html><head></head><body></body></html>")
      (is (equal :template (categorize-file template-file))))
    
    ;; Test that lock files are ignored
    (let* ((lock-filename ".#test.html")
           (lock-file (merge-pathnames lock-filename temp-dir)))
      (create-test-file lock-file "whatever")
      (is (null (categorize-file lock-file))))))

;;; Path handling tests

(test get-out-path-test
  "Test output path generation"
  ;; Set up the special variables in the die-trying package
  (setf die-trying::*input-dir* "www/")
  (setf die-trying::*output-dir* "out/")
  
  ;; Test basic path conversion
  (let ((input-path (pathname "www/test.html")))
    (let ((out-path (get-out-path input-path)))
      (is (str:containsp "out/" (namestring out-path)))
      (is (str:containsp "test.html" (namestring out-path)))))
  
  ;; Test nested path conversion
  (let ((input-path (pathname "www/pages/about.html")))
    (let ((out-path (get-out-path input-path)))
      (is (str:containsp "out/" (namestring out-path)))
      (is (str:containsp "pages/about.html" (namestring out-path))))))

;;; Fragment processing tests

(test build-fragment-objects-test
  "Test fragment object creation"
  (with-temp-directory (temp-dir)
    (let ((frag1 (format nil "~adt-header.html" temp-dir))
          (frag2 (format nil "~adt-footer.html" temp-dir)))
      (create-test-file frag1 "<header>Header</header>")
      (create-test-file frag2 "<footer>Footer</footer>")
      
      (let ((fragments (build-fragment-objects (list (pathname frag1) (pathname frag2)))))
        (is (= 2 (length fragments)))
        ;; Check that tags are extracted correctly
        (let ((tags (mapcar (lambda (f) (getf f :tag)) fragments)))
          (is (member "dt-header" tags :test #'string=))
          (is (member "dt-footer" tags :test #'string=)))))))

(test find-dependencies-test
  "Test dependency detection between fragments"
  (with-temp-directory (temp-dir)
    (let ((frag1 (format nil "~adt-header.html" temp-dir))
          (frag2 (format nil "~adt-page.html" temp-dir)))
      (create-test-file frag1 "<header>Header</header>")
      (create-test-file frag2 "<div><dt-header></dt-header></div>")
      
      (let* ((fragment-objs (build-fragment-objects (list (pathname frag1) (pathname frag2))))
             (with-deps (find-dependencies fragment-objs)))
        (is (= 2 (length with-deps)))
        ;; Check that dt-page depends on dt-header
        (let ((page-frag (find-if (lambda (f) (string= "dt-page" (getf f :tag))) with-deps)))
          (is (not (null page-frag)))
          (is (member "dt-header" (getf page-frag :depends-on) :test #'string=)))))))

;;; End-to-end integration tests

(test e2e-simple-build-test
  "Test a complete build process with simple fragments and templates"
  (with-temp-directory (temp-dir)
    (let* ((input-dir (format nil "~awww/" temp-dir))
           (output-dir (format nil "~aout/" temp-dir))
           (fragments-dir (format nil "~afragments/" input-dir))
           (assets-dir (format nil "~aassets/" input-dir)))
      
      ;; Create directory structure
      (ensure-directories-exist fragments-dir)
      (ensure-directories-exist assets-dir)
      
      ;; Create a simple fragment
      (create-test-file (format nil "~adt-header.html" fragments-dir)
                        "<header><h1>My Site</h1></header>")
      
      ;; Create a simple template
      (create-test-file (format nil "~aindex.html" input-dir)
                        "<!DOCTYPE html><html><head><title>Test</title></head><body><dt-header></dt-header><p>Content</p></body></html>")
      
      ;; Create an asset file
      (create-test-file (format nil "~astyle.css" assets-dir)
                        "body { margin: 0; }")
      
      ;; Run the build process
      (process input-dir output-dir)
      
      ;; Verify output files exist
      (is (file-exists-p (format nil "~aindex.html" output-dir)))
      (is (file-exists-p (format nil "~aassets/style.css" output-dir)))
      
      ;; Verify the content was processed
      (let ((output-content (read-file-content (format nil "~aindex.html" output-dir))))
        (is (str:containsp "My Site" output-content))
        (is (str:containsp "Content" output-content))))))

(test e2e-nested-fragments-test
  "Test build with nested fragment dependencies"
  (with-temp-directory (temp-dir)
    (let* ((input-dir (format nil "~awww/" temp-dir))
           (output-dir (format nil "~aout/" temp-dir))
           (fragments-dir (format nil "~afragments/" input-dir)))
      
      ;; Create directory structure
      (ensure-directories-exist fragments-dir)
      
      ;; Create nested fragments: dt-logo (leaf), dt-header (uses dt-logo), template (uses dt-header)
      (create-test-file (format nil "~adt-logo.html" fragments-dir)
                        "<img src='logo.png' alt='Logo' />")
      
      (create-test-file (format nil "~adt-header.html" fragments-dir)
                        "<header><dt-logo></dt-logo><h1>My Site</h1></header>")
      
      (create-test-file (format nil "~aindex.html" input-dir)
                        "<!DOCTYPE html><html><body><dt-header></dt-header></body></html>")
      
      ;; Run the build process
      (process input-dir output-dir)
      
      ;; Verify output exists and contains all nested content
      (is (file-exists-p (format nil "~aindex.html" output-dir)))
      (let ((output-content (read-file-content (format nil "~aindex.html" output-dir))))
        (is (str:containsp "logo.png" output-content))
        (is (str:containsp "My Site" output-content))))))

(test e2e-multiple-templates-test
  "Test build with multiple templates using shared fragments"
  (with-temp-directory (temp-dir)
    (let* ((input-dir (format nil "~awww/" temp-dir))
           (output-dir (format nil "~aout/" temp-dir))
           (fragments-dir (format nil "~afragments/" input-dir)))
      
      ;; Create directory structure
      (ensure-directories-exist fragments-dir)
      
      ;; Create shared fragments
      (create-test-file (format nil "~adt-header.html" fragments-dir)
                        "<header>Header</header>")
      
      (create-test-file (format nil "~adt-footer.html" fragments-dir)
                        "<footer>Footer</footer>")
      
      ;; Create multiple templates
      (create-test-file (format nil "~aindex.html" input-dir)
                        "<!DOCTYPE html><html><body><dt-header></dt-header><p>Home</p><dt-footer></dt-footer></body></html>")
      
      (create-test-file (format nil "~aabout.html" input-dir)
                        "<!DOCTYPE html><html><body><dt-header></dt-header><p>About</p><dt-footer></dt-footer></body></html>")
      
      ;; Run the build process
      (process input-dir output-dir)
      
      ;; Verify both templates were processed
      (is (file-exists-p (format nil "~aindex.html" output-dir)))
      (is (file-exists-p (format nil "~aabout.html" output-dir)))
      
      ;; Verify content in both files
      (let ((home-content (read-file-content (format nil "~aindex.html" output-dir)))
            (about-content (read-file-content (format nil "~aabout.html" output-dir))))
        (is (str:containsp "Header" home-content))
        (is (str:containsp "Home" home-content))
        (is (str:containsp "Footer" home-content))
        (is (str:containsp "Header" about-content))
        (is (str:containsp "About" about-content))
        (is (str:containsp "Footer" about-content))))))

(test e2e-asset-copying-test
  "Test that assets are copied correctly"
  (with-temp-directory (temp-dir)
    (let* ((input-dir (format nil "~awww/" temp-dir))
           (output-dir (format nil "~aout/" temp-dir))
           (assets-dir (format nil "~aassets/" input-dir))
           (css-dir (format nil "~acss/" assets-dir))
           (img-dir (format nil "~aimages/" assets-dir)))
      
      ;; Create directory structure
      (ensure-directories-exist css-dir)
      (ensure-directories-exist img-dir)
      
      ;; Create asset files
      (create-test-file (format nil "~astyle.css" css-dir)
                        "body { color: blue; }")
      
      (create-test-file (format nil "~alogo.svg" img-dir)
                        "<svg></svg>")
      
      ;; Create a simple template so process runs
      (create-test-file (format nil "~aindex.html" input-dir)
                        "<!DOCTYPE html><html><body>Test</body></html>")
      
      ;; Run the build process
      (process input-dir output-dir)
      
      ;; Verify assets were copied
      (is (file-exists-p (format nil "~aassets/css/style.css" output-dir)))
      (is (file-exists-p (format nil "~aassets/images/logo.svg" output-dir)))
      
      ;; Verify content is preserved
      (let ((css-content (read-file-content (format nil "~aassets/css/style.css" output-dir))))
        (is (str:containsp "color: blue" css-content))))))

;;; Convenience function to run all tests
(defun run-tests ()
  "Run all tests in the die-trying test suite."
  (run! 'die-trying-tests))

