(asdf:defsystem "www"
  :version "0.1.0"
  :author "Ian S. Pringle <me@die-trying.lol"
  :license "Unlicense"
  :serial t
  :depends-on (:flute)
  :components ((:module "www"
                :components ((:file "main")))))
