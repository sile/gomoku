(defpackage gomoku.trie.node-allocator
  (:use :common-lisp)
  (:export make
           allocate))
(in-package :gomoku.trie.node-allocator)

(declaim (inline can-allocate? allocate-impl))

(defstruct alloca
  (nexts #() :type (simple-array fixnum))
  (prevs #() :type (simple-array fixnum))
  (base-used? #() :type simple-bit-vector))

(defun make (node-count-limit)
  (let ((nexts (make-array node-count-limit :element-type 'fixnum))
        (prevs (make-array node-count-limit :element-type 'fixnum)))
    (loop FOR i FROM 0 BELOW node-count-limit
      DO
      (setf (aref nexts i) (1+ i)
            (aref prevs i) (1- i)))

    (let ((base-used? (make-array node-count-limit :element-type 'bit :initial-element 0)))
      (setf (bit base-used? 0) 1)

      (make-alloca :nexts nexts
                   :prevs prevs
                   :base-used? base-used?))))

(defun allocate-impl (alloca idx)
  (declare #.gomoku::*fastest*)
  (with-slots (nexts prevs) (the alloca alloca)
    (assert (and (/= -1 (aref nexts idx))
                 (/= -1 (aref prevs idx))))
    (setf (aref nexts (aref prevs idx)) (aref nexts idx)
          (aref prevs (aref nexts idx)) (aref prevs idx)
          (aref nexts idx) -1
          (aref prevs idx) -1)))

(defun can-allocate? (alloca base codes)
  (declare #.gomoku::*fastest*)
  (with-slots (nexts) (the alloca alloca)
    (every (lambda (cd) 
             (declare (fixnum base cd))
             (/= (aref nexts (+ base cd)) -1))
           (the list codes))))

(defun allocate (alloca codes)
  (declare #.gomoku::*fastest*)
  (with-slots (nexts base-used?) (the alloca alloca)
    (loop WITH first = (the fixnum (first codes))
          FOR cur = (the fixnum (aref nexts 0)) THEN (aref nexts cur)
          FOR base OF-TYPE fixnum = (- cur first)
          WHEN (>= base 0)
      DO
      (when (and (= (bit base-used? base) 0)
                 (can-allocate? alloca base codes))
        (setf (bit base-used? base) 1)
        
        (dolist (cd codes)
          (allocate-impl alloca (+ base cd)))
        (return base)))))
