(asdf:defsystem "die-trying"
  :version "0.1.0"
  :author "Ian S. Pringle <me@die-trying.lol"
  :license "Unlicense"
  :serial t
  :depends-on (:cl-fad
               :str
               :hunchentoot
               :file-notify
               :spinneret)
  :components ((:file "package")
               (:module "src"
                :components ((:file "utils")
                             (:file "dev")
                             (:file "main")))
               (:module "fragments"
                :components ((:file "with-page"))))
  :description "A blog and templating system.")
