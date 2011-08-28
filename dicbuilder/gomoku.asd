(in-package :asdf)

(defsystem gomoku
  :name "gomoku"
  :version "0.0.5"
  :author "Takeru Ohta"
  :description ""
  :depends-on (:dawg)
  :serial t
  
  :components ((:file "package")
               (:file "util")
               (:file "gomoku")))
