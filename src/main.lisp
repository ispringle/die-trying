(in-package #:die-trying)
(ql:quickload 'www)

(defun process (&optional (in "www/") (out "out/"))
  "The `process' function iterates over the files in `in' and ignores, copies, or renders them into `out'"
  (defparameter *input-dir* in)
  (defparameter *output-dir* out)
  (mapc-walk-directory
   (lambda (path)
     (cond
       ((str:starts-with-p ".#" (pathname-name path)) nil) ; Ignore emacs temp files
       ((not (equal (pathname-type path) "lisp")) (copy-file path)) ; All non-lisp files are copied to `out/' verbatim
       (t (load path))))
   in)
  (defparameter foo (make-hash-table))
  (maphash #'(lambda (uri page)
               (write-file (funcall page) uri))
           ;; foo)
           www:*renderables*)
  )
