(ql:quickload "lquery")
(ql:quickload "cl-fad")
(ql:quickload "str")

;;; Utils
(defun mapc-walk-directory (fn dir)
  "Walk a directory, `dir'` and run `fn' on all subdirectories and files."
  (dolist (entry (cl-fad:list-directory dir))
    (when (cl-fad:directory-pathname-p entry)
      (mapc-walk-directory fn entry))
    (funcall fn entry)))

(defun vector-empty-p (vec)
  (unless (vector-to-list vec) t))


(defun copy-file (path &optional (out (get-out-path path)))
  (unless (cl-fad:directory-exists-p out)
    (cl-fad:copy-file path (get-out-path path) :overwrite t)))

(defun get-out-path (file)
  (let* ((file-part (cadr (str:split *in* (namestring file))))
         (out (str:concat *out* file-part)))
    (ensure-directories-existy out)
    out))

(defun categorize-file (path)
  (if (not (equal (pathname-type path) "html"))
      'asset
      (let ((dom (lquery:$ (initialize path))))
        (if (vector-empty-p (lquery:$ dom "html"))
            'fragment
            'template))))

(defun path-to-dom (path)
  (lquery:$ (initialize path)))

(defun build-fragment-objects (paths)
  (mapcar (lambda (path)
            (let ((dom (path-to-dom path))
                  (tag (pathname-name path)))
              (list :dom dom :tag tag)))
          paths))

(defun expand (node fragments)
  (mapcar (lambda (fragment)
            (let ((tag (getf fragment ':tag))
                  (el (getf fragment ':dom)))
              (lquery:$ node tag (append el))))
          fragments)
  node)

;;; Processors
(defun process-input-files ()
  (let (templates fragments)
    (mapc-walk-directory (lambda (path)
                           (let ((category (categorize-file path)))
                             (cond ((equal category 'template) (setf templates (cons path templates)))
                                   ((equal category 'fragment) (setf fragments (cons path fragments)))
                                   (t (copy-file path)))))
                         *in*)
    (list templates fragments)))

(defun process (template-paths fragment-paths)
  (let ((fragments (build-fragment-objects fragment-paths)))
    (mapcar (lambda (path)
              (let ((dom (path-to-dom path))
                    (out (get-out-path path)))
                (lquery:$ dom (expand fragments) (write-to-file out))))
            template-paths)))

(defun main ()
  (let ((x (process-input-files)))
    (process (car x) (cadr x))))

(defparameter *in* "www/")
(defparameter *out* "out/")
(main)
