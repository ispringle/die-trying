(defpackage home.html
  (:use :cl :spinneret)
  (:export render))
(in-package :home.html)

(defun render ()
  (with-html-string
    (:div
     (:p "Hello world. This file was generated with Lisp. The HTML is rendered with Spinneret."))))
