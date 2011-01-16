(in-package :asdf)

(defsystem gomoku
  :name "gomoku"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description ""
  
  :serial t
  
  :components ((:file "package")
               (:file "util")
               (:file "trie/code-stream")
               (:file "trie/trie")
               (:file "trie/node-allocator")
               (:file "trie/double-array")
               (:file "gomoku")))
