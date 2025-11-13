;;; Build script for die-trying
(format t "~%Loading Quicklisp...~%")
(load "~/quicklisp/setup.lisp")

(format t "~%Loading die-trying system...~%")
(push #P"/app/" asdf:*central-registry*)
(ql:quickload :die-trying)

(format t "~%Running build process...~%")
(die-trying:process "www/" "out/")

(format t "~%Build complete!~%")
(format t "~%Checking output directory...~%")
(when (probe-file "/app/out/")
  (format t "Output directory exists!~%")
  (format t "Contents:~%")
  (dolist (file (directory "/app/out/**/*.*"))
    (format t "  ~A~%" file)))

(format t "~%Exiting...~%")
(quit)

