;;;; schoop.asd

(asdf:defsystem #:schoop
  :description "crappy snake game"
  :author "Jon"
  :license  "Do whatever"
  :version "0.0.1"
  :serial t
  :depends-on (#:lispbuilder-sdl)
  :components ((:file "package")
               (:file "schoop")))
