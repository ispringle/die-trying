(in-package #:die-trying)

(defun mapc-walk-directory (fn dir)
  "Walk a directory, `dir'` and run `fn' on all files."
  (dolist (entry (cl-fad:list-directory dir))
    (if (cl-fad:directory-pathname-p entry)
        (mapc-walk-directory fn entry)
        (funcall fn entry))))

(defun copy-file (path &optional (out (get-out-path path)))
  "Copy file at `path' into `out' with new file path based on original."
  (unless (cl-fad:directory-exists-p out)
    (cl-fad:copy-file path (get-out-path path) :overwrite t)))

(defun write-file (s path &optional (out (get-out-path path)))
  "Create an html file in `out' with filepath based on original `path' and with
   contents `s'."
  (unless (cl-fad:directory-exists-p out)
    (let ((out-path (get-out-path path t)))
      (str:to-file out-path s))))

(defun get-out-path (file &optional template)
  "Determine the output filepath based on the `file's current path."
  (let* ((file-part (cadr (str:split *input-dir* (namestring file))))
         (out (str:concat *output-dir* (if template
                                           (pathname-name file-part)
                                           file-part))))
    (ensure-directories-exist out)
    out))

(defun get-package-name (path)
  "Infers a CL package name from the path, where the name is the relative path
   of the file from `*input-dir*' sans the file extension."
  (pathname-name (cadr (str:split *input-dir* (namestring path)))))
