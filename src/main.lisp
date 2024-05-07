(defpackage die-trying
  (:use :cl)
  (:export :start-dev :process))

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
    ((str:starts-with-p "#." (pathname-name path)) nil)
    ((not (equal (pathname-type path) "html")) 'asset)
    (t (let ((dom (lquery:$ (initialize path))))
         (if (vector-empty-p (lquery:$ dom "html"))
             'fragment
             'template)))))

(defun path-to-dom (path &optional (fragment nil))
  (if fragment
      (lquery:$ (initialize path) (children))
      (lquery:$ (initialize path))))

(defun build-fragment-objects (paths)
  (mapcar (lambda (path)
            (let ((dom (path-to-dom path t))
                  (tag (pathname-name path)))
              (list :dom dom :tag tag)))
          paths))

(defun find-dependencies (fragments)
  (let (updated-fragments)
    (mapcar (lambda (dom-plist)
              (let ((dependencies nil)
                    (dom (getf dom-plist ':dom)))
                (mapcar (lambda (fragment)
                          (let ((tag (getf fragment ':tag)))
                            (when (not (vector-empty-p (lquery:$ dom tag)))
                              (setf dependencies (cons tag dependencies)))))
                        fragments)
                (setf updated-fragments
                      (cons (list :tag (getf dom-plist ':tag)
                                  :dom dom
                                  :deps dependencies)
                            updated-fragments))))
            fragments)
    updated-fragments))

(defun expand (node fragments)
  (mapcar (lambda (fragment)
            (let ((tag (getf fragment ':tag))
                  (el (getf fragment ':dom)))
              (lquery:$ node tag (append el))))
          fragments)
  node)

;;; Processors
(defun process-input-files (dir)
  (let (templates fragments)
    (mapc-walk-directory (lambda (path)
                           (let ((category (categorize-file path)))
                             (cond ((equal category 'template) (setf templates (cons path templates)))
                                   ((equal category 'fragment) (setf fragments (cons path fragments)))
                                   ((equal category 'asset) (copy-file path))
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
         (expanded-fragments nil))
    (mapcar (lambda (fragment)
              (setf expanded-fragments
                    (cons (list :tag (getf fragment ':tag)
                                :dom (expand (getf fragment ':dom) fragments))
                          expanded-fragments)))
            fragments)
    expanded-fragments))

(defun process (&optional (in "www/") (out "out/"))
  (defparameter *input-dir* in)
  (defparameter *output-dir* out)
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
