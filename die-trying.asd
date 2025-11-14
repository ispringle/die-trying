(asdf:defsystem "die-trying"
  :version "0.1.0"
  :author "Ian S. Pringle <me@die-trying.lol"
  :license "Unlicense"
  :depends-on (:lquery
               :cl-fad
               :str
               :hunchentoot
               :file-notify
               :spinneret)
  :components ((:file "main"))
  :description "A simple HTML templating system.")

(asdf:defsystem "die-trying-tests"
  :version "0.1.0"
  :author "Ian S. Pringle <me@die-trying.lol"
  :license "Unlicense"
  :depends-on (:die-trying
               :fiveam
               :cl-fad
               :str)
  :components ((:file "tests"))
  :description "Test suite for die-trying."
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :fiveam :run! :die-trying-tests)))
