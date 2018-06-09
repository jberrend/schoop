;;;; schoop.asd

(asdf:defsystem #:schoop
  :description "Describe schoop here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:lispbuilder-sdl)
  :components ((:file "package")
               (:file "schoop")))
