(defpackage node-allocator
  (:use :common-lisp)
  (:export make
           allocate))
(in-package :node-allocator)

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
    (make-alloca :nexts nexts
                 :prevs prevs
                 :base-used? (make-array node-count-limit :element-type 'bit :initial-element 0))))

(defun allocate-impl (alloca idx)
  (with-slots (nexts prevs) (the alloca alloca)
    (assert (and (/= -1 (aref nexts idx))
                 (/= -1 (aref prevs idx))))
    (setf (aref nexts (aref prevs idx)) (aref nexts idx)
          (aref prevs (aref nexts idx)) (aref prevs idx)
          (aref nexts idx) -1
          (aref prevs idx) -1)))

(defun can-allocate? (alloca base codes)
  (with-slots (nexts) (the alloca alloca)
    (every (lambda (cd) 
             (/= (aref nexts (+ base cd)) -1))
           codes)))

(defun allocate (alloca codes)
  (with-slots (nexts base-used?) (the alloca alloca)
    #+IGNORE
    (print `(:head ,(aref nexts 0)))
  (when (zerop (random 1000))
    (print (list (aref nexts 0) (length codes))))

    (loop WITH first = (first codes)
          FOR cur = (aref nexts 0) THEN (aref nexts cur)
          FOR base = (- cur first)
          WHEN (>= base 0)
      DO
      (when (and (= (bit base-used? base) 0)
                 (can-allocate? alloca base codes))
        (setf (bit base-used? base) 1)
        ;;
        #+IGNORE
        (when (= (aref nexts 0) 4941)
          (print codes))

        (dolist (cd codes)
          (allocate-impl alloca (+ base cd)))
        (return base)))))
