;;;; die-trying.lisp
(load "die-trying.asd")
;; (ql:quickload "die-trying")

(in-package #:die-trying)

;;; Globals
(defvar *input-dir* "www/"
  "Directory containing all the HTML templates and fragments.")
(defvar *output-dir* "out/"
  "Directory to save compiled HTML files.")
(defvar *assets-dir* "www/assets"
  "Directory containing static assets to be copied to `*output-dir*'")

(defparameter *templates* nil
  "All HTML template files in `*input-dir*'")
(defparameter *fragments* nil
  "All HTML fragment filts in `*input-dir*'")

;;; Utility Functions
(defun mapc-walk-directory (fn dir)
  (dolist (entry (cl-fad:list-directory dir))
    (when (cl-fad:directory-pathname-p entry)
      (mapc-walk-directory fn entry))
    (funcall fn entry)))

(defun vector-to-list (vec)
  (coerce vec 'list))

(defun vector-empty-p (vec)
  (when (vector-to-list vec) t))

(defun vector-op (func vec)
  (funcall func (vector-to-list vec)))

;;; Do the things
(defun expand (node)
  (lambda (fragment)
    (destructuring-bind (&key tag el) fragment
      (lquery:$ node tag (append el)))
    node))

(defun expand-all (node fragments)
  (let ((expand-node (expand node)))
    (mapcar expand-node fragments)))

(defun process-dom (path)
  (let ((dom (lquery:$ (initialize path))))
    (if (vector-empty-p (lquery:$ dom "html"))
        (setf *templates* (cons path *templates*))
        (setf *fragments*
              (cons (list :tag (pathname-name path)
                          :el (vector-op 'car (lquery:$ dom(serialize))))
                    *fragments*)))))

(defun copy-file (path &optional (out (get-out-path path)))
  (unless (cl-fad:directory-exists-p out)
    (cl-fad:copy-file path (get-out-path path) :overwrite t)))

(defun process-file (file)
  (if (equal (pathname-type file) "html")
      (process-dom file)
      (copy-file file)))

(defun process-files ()
  (setf *templates* nil
        *fragments* nil)
  (mapc-walk-directory 'process-file *input-dir*))

(defun get-out-path (file)
  (let* ((file-part (cadr (str:split *input-dir* (namestring file))))
         (out (str:concat *output-dir* file-part)))
    (ensure-directories-exist out)
    out))

(defun process-templates ()
  (mapcar (lambda (file)
            (let ((dom (lquery:$ (initialize file)))
                  (out (get-out-path file)))
              ;; Process fragment tags
              (lquery:$ dom (expand-all *fragments*) (write-to-file out))))
          *templates*))

(defun main ()
  (process-files)
  (process-templates))
(main)
