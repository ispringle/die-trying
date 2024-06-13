(defpackage index.html (:use :cl :spinneret :dt/fragments :die-trying) (:export
#:render)) (in-package :index.html) (defun render () (with-page (:title "Die
Trying") (:div (:p "Hello, world!"))))
