;;;; orb.asd

(asdf:defsystem #:orb
  :description "Describe orb here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (:daft)
  :serial t
  :components ((:file "package")
               (:file "base")
               (:file "ship")))
