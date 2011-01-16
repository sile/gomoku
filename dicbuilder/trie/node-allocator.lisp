(defpackage node-allocator
  (:use :common-lisp)
  (:export make
           allocate))
(in-package :node-allocator)

(defun make (node-count-limit)
  node-count-limit)

(defun allocate (alloca codes)
  (declare (ignore alloca codes))
  0) 