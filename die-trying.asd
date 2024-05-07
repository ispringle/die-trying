(asdf:defsystem "die-trying"
  :version "0.1.0"
  :author "Ian S. Pringle <me@die-trying.lol"
  :license "Unlicense"
  :depends-on (:lquery
               :cl-fad
               :str
               :hunchentoot
               :file-notify)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A blog and templating system.")

