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
  (unless (vector-to-list vec) t))

(defun vector-op (func vec)
  (funcall func (vector-to-list vec)))

(defun dom-to-string (dom)
  (vector-op 'car (lquery:$ dom (serialize))))

;;; Do the things
(defun expand (node)
  (lambda (fragment)
    (let ((tag (getf fragment ':tag))
          (el (getf fragment ':el)))
      (lquery:$ node tag (append el)))
    node))

(defun expand-all (node fragments)
  (let ((expand-node (expand node)))
    (mapcar expand-node fragments)))

(defun process-doms (path)
  (let ((dom (lquery:$ (initialize path))))
    (if (not (vector-empty-p (lquery:$ dom "html")))
        (setf *templates* (cons path *templates*))
        (setf *fragments*
              (cons (list :tag (pathname-name path)
                          :el dom
                          :path path)
                    *fragments*)))))

(defun copy-file (path &optional (out (get-out-path path)))
  (unless (cl-fad:directory-exists-p out)
    (cl-fad:copy-file path (get-out-path path) :overwrite t)))

(defun process-file (file)
  (if (equal (pathname-type file) "html")
      (process-doms file)
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

(defun process-templates (templates fragments)
  (mapcar (lambda (file)
            (let ((dom (lquery:$ (initialize file)))
                  (out (get-out-path file)))
              (lquery:$ dom (expand-all fragments) (write-to-file out))))
          templates))

(defun add-dependencies (fragments)
  (let ((updated-fragments nil))
    (mapcar (lambda (frag)
              (setf updated-fragments
                    (cons (list :tag (getf frag ':tag)
                                :el (expand-all
                                     (getf frag ':el)
                                     fragments))
                          updated-fragments)))
            fragments)
    updated-fragments))

(defun list-dependencies (fragments)
  (let ((updated-fragments nil))
    (mapcar (lambda (dom-plist)
              (let ((dependencies nil)
                    (dom (getf dom-plist ':el)))
                (mapcar (lambda (frag)
                          (let ((tag (getf frag ':tag)))
                            (when (not (vector-empty-p (lquery:$ dom tag)))
                              (setf dependencies (cons tag dependencies)))))
                        fragments)
                (setf updated-fragments
                      (cons (list :tag (getf dom-plist ':tag)
                                  :deps dependencies
                                  :el dom)
                            updated-fragments))))
            fragments)
    updated-fragments))

(defun process-fragments (fragments)
  (add-dependencies 
   (list-dependencies fragments)))

(defun main ()
  (process-files)
  (process-templates *templates*
                     (process-fragments *fragments*)))
(main)
