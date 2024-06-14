(defpackage www
  (:use :cl :die-trying :flute)
  (:export :*renderables*))
(in-package #:www)

(defparameter *renderables* (make-hash-table :test 'equal))

(defmacro defpage (uri (&key title) &body body)
  `(setf (gethash ,uri *renderables*)
         (lambda ()
           (html
            (head
             (title ,title)
             (meta :name "viewport" :content "width=device-width,initial-scale=1")
             (link :rel "stylesheet"
                   :href "https://fonts.googleapis.com/css2?family=Oldenburg&display=swap")
             (link :rel "stylesheet"
                   :href "assets/style.css")
             (meta :name "description"
                   :content "Get rich or...")
             (meta :charset "utf-8"))
            (body ,@body)))))
