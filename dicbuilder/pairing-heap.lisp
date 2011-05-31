(defpackage :pairing-heap
  (:use :cl)
  (:shadow :push :pop)
  (:export :make
	   :push  
	   :pop
	   :empty?))
(in-package :pairing-heap)

;;; struct
(defstruct (heap (:constructor make (&key node (test #'<))))
  node
  (test #'<)) 

(defstruct node
  element
  childs)     ; list of node

;;; internal function
(defun merge-node (n1 n2 <)
  (cond ((null n1) n2)
	((null n2) n1)
	(t (if (funcall < #1=(node-element n1) #2=(node-element n2))
	       (make-node :element #1# :childs (cons n2 (node-childs n1)))
	     (make-node :element #2# :childs (cons n1 (node-childs n2)))))))

(defun merge-pairs (nodes <)
  (if (null (cdr nodes))
      (car nodes)
    (destructuring-bind (first second . rest) nodes
      (merge-node (merge-node first second <) 
		  (merge-pairs rest <)
		  <))))

;;; external function
(defun push (element heap) 
  (setf (heap-node heap) (merge-node (make-node :element element) 
				     (heap-node heap) 
				     (heap-test heap)))
  heap)

(defun pop (heap)
  (let ((root (heap-node heap)))
    (when root
      (prog1 (node-element root)
	(setf (heap-node heap) (merge-pairs (node-childs root) (heap-test heap)))))))

(defun empty? (heap)
  (null (heap-node heap)))
