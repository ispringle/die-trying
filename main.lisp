(defpackage die-trying
  (:use :cl)
  (:export :start-dev
           :process
           :watch
           ;; Utility functions (exported for testing)
           :vector-to-list
           :vector-empty-p
           :categorize-file
           :topological-sort
           :get-out-path
           :build-fragment-objects
           :find-dependencies
           ;; Dynamic content generation utilities
           :get-all-posts
           :get-post-metadata
           :format-date-pretty
           :generate-social-meta))

(in-package #:die-trying)

;;; Utils
(defun mapc-walk-directory (fn dir)
  "Walk a directory, `dir'` and run `fn' on all subdirectories and files."
  (dolist (entry (cl-fad:list-directory dir))
    (when (cl-fad:directory-pathname-p entry)
      (mapc-walk-directory fn entry))
    (funcall fn entry)))

(defun vector-empty-p (vec)
  (unless (vector-to-list vec) t))

(defun vector-to-list (vec)
  (coerce vec 'list))

(defun copy-file (path &optional (out (get-out-path path)))
  (unless (cl-fad:directory-exists-p out)
    (cl-fad:copy-file path (get-out-path path) :overwrite t)))

(defun get-out-path (file)
  (let* ((file-part (cadr (str:split *input-dir* (namestring file))))
         (out (str:concat *output-dir* file-part)))
    (ensure-directories-exist out)
    out))

(defun categorize-file (path)
  (cond
    ((str:starts-with-p ".#" (pathname-name path)) nil)
    ;; .lisp files in fragments/ are fragments, elsewhere they're ignored (already processed as templates)
    ((equal (pathname-type path) "lisp")
     (if (search "fragments" (namestring path))
         :fragment
         nil))
    ((not (equal (pathname-type path) "html")) :asset)
    (t (let ((dom (lquery:$ (initialize path))))
         (if (vector-empty-p (lquery:$ dom "html"))
             :fragment
             :template)))))

(defun path-to-dom (path &optional (fragment nil))
  (if fragment
      (lquery:$ (initialize path) (children))
      (lquery:$ (initialize path))))

(defun build-fragment-objects (paths)
  (mapcar (lambda (path)
            (let ((tag (pathname-name path)))
              (if (equal (pathname-type path) "lisp")
                  ;; For .lisp fragments, load and eval to get a function
                  (let ((fragment-fn (with-open-file (in path)
                                       (let ((result nil))
                                         (loop for form = (read in nil 'eof)
                                               until (eq form 'eof)
                                               do (setf result (eval form)))
                                         result))))
                    (list :function fragment-fn :tag tag :type :lisp))
                  ;; For .html fragments, parse DOM as before
                  (let ((dom (path-to-dom path t)))
                    (list :dom dom :tag tag :type :html)))))
          paths))

(defun find-dependencies (fragments)
  (let (updated-fragments)
    (mapcar (lambda (fragment-plist)
              (let ((dependencies nil)
                    (frag-type (getf fragment-plist ':type)))
                ;; Only check dependencies for HTML fragments
                (when (equal frag-type :html)
                  (let ((dom (getf fragment-plist ':dom)))
                    (mapcar (lambda (other-fragment)
                              (let ((tag (getf other-fragment ':tag)))
                                (when (not (vector-empty-p (lquery:$ dom tag)))
                                  (setf dependencies (cons tag dependencies)))))
                            fragments)))
                ;; Build updated fragment with dependencies
                (setf updated-fragments
                      (cons (append fragment-plist (list :depends-on dependencies))
                            updated-fragments))))
            fragments)
    updated-fragments))

(defun node-children-to-str (node)
  (car (vector-to-list (lquery:$ node (text)))))

(defun topological-sort (fragments)
  "Sort fragments by dependencies so leaves (no deps) come first."
  (let ((sorted nil)
        (remaining (copy-list fragments)))
    (loop while remaining do
      (let ((no-deps (remove-if (lambda (frag)
                                  ;; Has dependencies that aren't yet in sorted
                                  (some (lambda (dep)
                                          (not (member dep (mapcar (lambda (f) (getf f ':tag)) sorted)
                                                          :test #'string=)))
                                        (getf frag ':depends-on)))
                                remaining)))
        (if no-deps
            (progn
              (setf sorted (append sorted no-deps))
              (setf remaining (set-difference remaining no-deps)))
            (progn
              ;; Circular dependency or other issue - just append remaining and break
              (setf sorted (append sorted remaining))
              (setf remaining nil)))))
    sorted))

(defun extract-page-metadata (dom)
  "Extract metadata from a page's DOM for passing to fragments.
   Only extracts if a .meta wrapper exists - returns nil values otherwise
   to allow fragments to use their defaults."
  (let ((meta-elements (lquery:$ dom ".meta")))
    (if (vector-empty-p meta-elements)
        ;; No .meta wrapper - return nil values for all (triggers defaults)
        (list :title nil
              :description nil
              :summary nil
              :excerpt nil
              :date nil
              :author nil
              :url nil
              :image nil)
        ;; Has .meta wrapper - extract metadata from it and nearby content
        (let* (;; Title: check data-title attribute first, then .p-name
               (data-title-nodes (lquery:$ dom "[data-title]" (attr "data-title")))
               (name-nodes (lquery:$ dom "dt-article .p-name" (text)))
               (title (cond
                        ((and data-title-nodes (not (vector-empty-p data-title-nodes)))
                         (aref data-title-nodes 0))
                        ((and name-nodes (not (vector-empty-p name-nodes)))
                         (aref name-nodes 0))
                        (t nil)))
               ;; Other metadata from .meta wrapper
               (summary-nodes (lquery:$ dom ".meta .p-summary" (text)))
               (summary (if (not (vector-empty-p summary-nodes))
                            (aref summary-nodes 0)
                            nil))
               (excerpt-nodes (lquery:$ dom ".meta .p-excerpt" (text)))
               (excerpt (if (not (vector-empty-p excerpt-nodes))
                            (aref excerpt-nodes 0)
                            nil))
               (date-nodes (lquery:$ dom ".meta .dt-published" (attr "datetime")))
               (date (if (not (vector-empty-p date-nodes))
                         (aref date-nodes 0)
                         nil))
               (author-nodes (lquery:$ dom ".meta .p-author" (text)))
               (author (if (not (vector-empty-p author-nodes))
                           (aref author-nodes 0)
                           nil))
               (url-nodes (lquery:$ dom ".meta .u-url" (attr "href")))
               (url (if (not (vector-empty-p url-nodes))
                        (aref url-nodes 0)
                        nil))
               (featured-nodes (lquery:$ dom ".meta .u-featured" (attr "src")))
               (featured (if (not (vector-empty-p featured-nodes))
                             (aref featured-nodes 0)
                             nil)))
          ;; Return as plist
          (list :title title
                :description (or excerpt summary)
                :summary summary
                :excerpt excerpt
                :date date
                :author author
                :url url
                :image featured)))))

(defun filter-nil-metadata (plist)
  "Remove keys with nil values from a plist so defaults can be used."
  (let ((result nil))
    (loop for (key value) on plist by #'cddr
          when value
          do (setf result (append result (list key value))))
    result))

(defun expand (node fragments &optional (metadata nil))
  "Expand fragments in node, passing metadata to Lisp fragments."
  (let* ((page-metadata (or metadata (extract-page-metadata node)))
         ;; Filter out nil values so fragment defaults are used
         (filtered-metadata (filter-nil-metadata page-metadata)))
    (mapcar (lambda (fragment)
              (let ((tag (getf fragment ':tag))
                    (frag-type (getf fragment ':type)))
                (cond
                  ;; HTML fragments: prepend DOM as before
                  ((equal frag-type :html)
                   (let ((el (getf fragment ':dom)))
                     (lquery:$ node tag (prepend el))))
                  ;; Lisp fragments: call function with metadata and insert HTML
                  ((equal frag-type :lisp)
                   (let* ((fragment-fn (getf fragment ':function))
                          (html-output (when (functionp fragment-fn)
                                         (apply fragment-fn filtered-metadata))))
                     (when (stringp html-output)
                       (lquery:$ node tag (prepend html-output)))))
                  ;; Fallback for old-style fragments without type
                  (t
                   (when (getf fragment ':dom)
                     (lquery:$ node tag (prepend (getf fragment ':dom))))))))
            fragments)
    node))

;;; Dynamic content generation utilities

(defun get-all-posts (dir)
  "Get all post files from specified directory, sorted by date (newest first)."
  (let ((posts nil)
        (search-path (merge-pathnames "*.html" dir)))
    (when (probe-file dir)
      (dolist (file (directory search-path))
        (push file posts)))
    (sort posts #'string> :key #'namestring)))

(defun extract-date-from-filename (filename)
  "Extract date from filename like 'workout-2024-06-17.html' -> '2024-06-17'."
  (let* ((name (pathname-name filename))
         (parts (str:split "-" name)))
    (when (>= (length parts) 3)
      (let ((year (nth (- (length parts) 3) parts))
            (month (nth (- (length parts) 2) parts))
            (day (nth (- (length parts) 1) parts)))
        (when (and (every #'digit-char-p year)
                   (every #'digit-char-p month)
                   (every #'digit-char-p day))
          (format nil "~a-~a-~a" year month day))))))

(defun parse-date-string (date-str)
  "Parse YYYY-MM-DD or ISO 8601 datetime string into (year month day)."
  (when date-str
    ;; Extract just the date portion if this is an ISO 8601 datetime
    (let* ((date-only (if (search "T" date-str)
                          (subseq date-str 0 (position #\T date-str))
                          date-str))
           (parts (str:split "-" date-only)))
      (when (= (length parts) 3)
        (handler-case
            (mapcar #'parse-integer parts)
          (error () nil))))))

(defun format-date-pretty (date-str)
  "Format YYYY-MM-DD as 'Month DDth, YYYY'."
  (let* ((date-parts (parse-date-string date-str))
         (months '("January" "February" "March" "April" "May" "June" 
                   "July" "August" "September" "October" "November" "December")))
    (when date-parts
      (let* ((year (first date-parts))
             (month (second date-parts))
             (day (third date-parts))
             (month-name (nth (1- month) months))
             (day-suffix (cond
                           ((or (= day 11) (= day 12) (= day 13)) "th")
                           ((= (mod day 10) 1) "st")
                           ((= (mod day 10) 2) "nd")
                           ((= (mod day 10) 3) "rd")
                           (t "th"))))
        (format nil "~a ~d~a, ~d" month-name day day-suffix year)))))

(defun get-post-metadata (post-file)
  "Extract metadata from a post file including title, date, excerpt, and microformat data."
  (let* ((dom (lquery:$ (initialize post-file)))
         ;; Extract date from <time datetime> or filename
         (date-from-filename (extract-date-from-filename post-file))
         (datetime-nodes (lquery:$ dom ".dt-published" (attr "datetime")))
         (date (if (and datetime-nodes (not (vector-empty-p datetime-nodes)))
                   (aref datetime-nodes 0)
                   date-from-filename))
         ;; Extract title from .p-name or <h2>
         (title-nodes (lquery:$ dom ".p-name" (text)))
         (h2-nodes (lquery:$ dom "h2" (text)))
         (title (cond
                  ((and title-nodes (not (vector-empty-p title-nodes)))
                   (aref title-nodes 0))
                  ((and h2-nodes (not (vector-empty-p h2-nodes)))
                   (aref h2-nodes 0))
                  (t (pathname-name post-file))))
         ;; Extract excerpt - check .p-excerpt first, then .p-summary, then first <p>
         (excerpt-nodes (lquery:$ dom ".p-excerpt" (text)))
         (summary-nodes (lquery:$ dom ".p-summary" (text)))
         (p-nodes (lquery:$ dom "p" (text)))
         (excerpt (cond
                    ((and excerpt-nodes (not (vector-empty-p excerpt-nodes)))
                     (aref excerpt-nodes 0))
                    ((and summary-nodes (not (vector-empty-p summary-nodes)))
                     (aref summary-nodes 0))
                    ((and p-nodes (not (vector-empty-p p-nodes)))
                     (let ((text (aref p-nodes 0)))
                       (if (> (length text) 200)
                           (format nil "~a..." (subseq text 0 197))
                           text)))
                    (t "")))
         ;; Extract URL from .u-url
         (url-nodes (lquery:$ dom ".u-url" (attr "href")))
         (url (if (and url-nodes (not (vector-empty-p url-nodes)))
                  (aref url-nodes 0)
                  (format nil "/~a.html" (pathname-name post-file))))
         ;; Extract featured image from .u-featured
         (featured-nodes (lquery:$ dom ".u-featured" (attr "src")))
         (featured (when (and featured-nodes (not (vector-empty-p featured-nodes)))
                     (aref featured-nodes 0))))
    (list :file post-file
          :tag (pathname-name post-file)
          :title title
          :date date
          :date-pretty (format-date-pretty date)
          :excerpt excerpt
          :url url
          :featured featured)))

(defun generate-social-meta (&key title description image url (type "website"))
  "Generate Open Graph and Twitter Card meta tags as an HTML string."
  (spinneret:with-html-string
    ;; Open Graph tags
    (when title
      (:meta :property "og:title" :content title))
    (when description
      (:meta :property "og:description" :content description))
    (when image
      (:meta :property "og:image" :content image))
    (when url
      (:meta :property "og:url" :content url))
    (when type
      (:meta :property "og:type" :content type))
    
    ;; Twitter Card tags
    (:meta :name "twitter:card" :content "summary_large_image")
    (when title
      (:meta :name "twitter:title" :content title))
    (when description
      (:meta :name "twitter:description" :content description))
    (when image
      (:meta :name "twitter:image" :content image))))

;;; Processors

(defun process-lisp-templates (dir)
  "Process all .lisp template files (excluding fragments/) and generate corresponding .html files."
  (let ((generated-files nil))
    (mapc-walk-directory 
     (lambda (path)
       (when (and (equal (pathname-type path) "lisp")
                  (not (search "fragments" (namestring path))))
         (handler-case
             (let* ((html-output (with-open-file (in path)
                                   (let ((result nil))
                                     (loop for form = (read in nil 'eof)
                                           until (eq form 'eof)
                                           do (setf result (eval form)))
                                     result)))
                    (html-path (make-pathname :type "html" :defaults path)))
               (if (stringp html-output)
                   (progn
                     (with-open-file (out html-path
                                          :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
                       (write-string html-output out))
                     (push html-path generated-files)
                     (format t "Generated ~a from ~a~%" html-path path))
                   (format t "WARNING: ~a did not produce a string output~%" path)))
           (error (e)
             (format t "Error processing ~a: ~a~%" path e)))))
     dir)
    generated-files))

(defun process-input-files (dir)
  (let (templates fragments)
    (mapc-walk-directory (lambda (path)
                           (let ((category (categorize-file path)))
                             (cond ((equal category :template) (setf templates (cons path templates)))
                                   ((equal category :fragment) (setf fragments (cons path fragments)))
                                   ((equal category :asset) (copy-file path))
                                   (t) (nil))))
                         dir)
    (list templates fragments)))

(defun process-templates (templates fragments)
  (mapcar (lambda (path)
            (let ((dom (path-to-dom path))
                  (out (get-out-path path)))
              (lquery:$ dom (expand fragments) (write-to-file out))))
          templates))

(defun process-fragments (fragment-paths)
  (let* ((fragments (build-fragment-objects fragment-paths))
         (fragments (find-dependencies fragments))
         (sorted-fragments (topological-sort fragments))
         (expanded-fragments nil))
    ;; Expand in dependency order - leaves first
    (mapcar (lambda (fragment)
              (let ((frag-type (getf fragment ':type)))
                (cond
                  ;; HTML fragments: expand with other fragments
                  ((equal frag-type :html)
                   (let ((expanded-dom (expand (getf fragment ':dom) expanded-fragments)))
                     (setf expanded-fragments
                           (cons (list :tag (getf fragment ':tag)
                                       :dom expanded-dom
                                       :type :html)
                                 expanded-fragments))))
                  ;; Lisp fragments: just pass through (they're already functions)
                  ((equal frag-type :lisp)
                   (setf expanded-fragments
                         (cons (list :tag (getf fragment ':tag)
                                     :function (getf fragment ':function)
                                     :type :lisp)
                               expanded-fragments)))
                  ;; Fallback for compatibility
                  (t
                   (let ((expanded-dom (expand (getf fragment ':dom) expanded-fragments)))
                     (setf expanded-fragments
                           (cons (list :tag (getf fragment ':tag)
                                       :dom expanded-dom)
                                 expanded-fragments)))))))
            sorted-fragments)
    expanded-fragments))

(defun process (&optional (in "www/") (out "out/"))
  (defparameter *input-dir* in)
  (defparameter *output-dir* out)
  ;; First, process .lisp templates to generate .html files
  (process-lisp-templates in)
  ;; Then process all HTML files (including generated ones)
  (let* ((x (process-input-files in))
         (fragments (process-fragments (cadr x))))
    (process-templates (car x) fragments)))

;; Dev server
(defparameter *acceptor* nil)
(defparameter *port* 4321)

(defun file-watcher (path func)
  "Starts a file-notify watcher in a thread to watch `path' and run `func' when a file changes."
  (format t "Starting file watcher on ~a~%" path)
  (org.shirakumo.file-notify:watch path)
  (org.shirakumo.file-notify:with-events (file change :timeout t)
    (when (not (str:starts-with-p ".#" (pathname-name file)))
      (format t "~a changed~%" file)
      (funcall func file change))))

(defun watch ()
  (file-watcher "www/" (lambda (file change)
                         (declare (ignore file change))
                         (process))))

(defun serve ()
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                  :port *port*
                                  :document-root *output-dir*))
  (format t "Starting development server on ~a~%." *port*)
  (hunchentoot:start *acceptor*))

(defun start-dev ()
  (process)
  (serve)
  (watch))
