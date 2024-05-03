;;;; die-trying.asd

(asdf:defsystem #:die-trying
  :description "Describe die-trying here"
  :author "Ian S. Pringle <me@die-trying.lol>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:lquery
               #:cl-fad
               #:str)
  :components ((:file "package")
               (:file "die-trying")))
