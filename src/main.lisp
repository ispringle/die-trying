(in-package #:die-trying)

;;; Spinneret config 
(defparameter spinneret:*html-style* :tree)
(push "x-" spinneret:*unvalidated-attribute-prefixes*) ; alpinejs
(push "@" spinneret:*unvalidated-attribute-prefixes*) ; alpinejs
(push "hx-" spinneret:*unvalidated-attribute-prefixes*) ; htmx

(defun process (&optional (in "www/") (out "out/"))
  "The `process' function iterates over the files in `in' and ignores, copies, or renders them into `out'"
  (defparameter *input-dir* in)
  (defparameter *output-dir* out)
  (mapc-walk-directory
   (lambda (path)
     (cond
       ((str:starts-with-p ".#" (pathname-name path)) nil) ; Ignore emacs temp files
       ((not (equal (pathname-type path) "lisp")) (copy-file path)) ; All non-lisp files are copied to `out/' verbatim
       (t (let* ((loaded (load path)) ; All remaining lisp files are pages to be rendere
                 (pkg (unless (not loaded)
                        (find-package (string-upcase (get-package-name path)))))
                 (render-func (unless (null pkg)
                                (find-symbol (string-upcase "render") pkg))))
            (unless (null render-func)
              (write-file (funcall render-func) path))))))
   in))
