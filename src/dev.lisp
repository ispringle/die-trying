(in-package #:die-trying)

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

(defun watch (&optional (in "www/"))
  "Watch `in' for file changes and rerun `process' on change."
  (file-watcher in (lambda (file change)
                     (declare (ignore file change))
                     (process))))

(defun serve (&optional (out "out/"))
  "Serve files in `out/' for local development."
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                  :port *port*
                                  :document-root out))
  (format t "Starting development server on ~a~%." *port*)
  (hunchentoot:start *acceptor*))

(defun start-dev ()
  "Initializes development processes."
  (process)
  (serve)
  (watch))
