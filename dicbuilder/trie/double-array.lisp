(defpackage double-array
  (:use :common-lisp :gomoku)
  (:export from-trie))
(in-package :double-array)

(defstruct da
  (base #() :type (simple-array (unsigned-byte 32)))
  (chck #() :type (simple-array (unsigned-byte 16)))
  (opts #() :type (simple-array (unsigned-byte 32))))

(defmacro a.if (exp then else)
  `(let ((it ,exp))
     (if it
         ,then
       ,else)))

;; XXX:
(defun node-count (da)
  (length (da-base da)))

(defun node-count-limit (trie &aux (map (make-hash-table :test #'eq)))
  (labels ((recur (node)
             (unless (gethash node map)
               (setf (gethash node map) t)
               (dolist (child (trie::collect-children node))
                 (recur child)))))
    (recur trie)
    (round (* 4 (hash-table-count map)))))

(defun init-da (node-count-limit)
  (make-da :base (make-array node-count-limit :element-type '(unsigned-byte 32) :initial-element 0)
           :chck (make-array node-count-limit :element-type '(unsigned-byte 16) :initial-element #xFFFF)
           :opts (make-array node-count-limit :element-type '(unsigned-byte 32) :initial-element 0)))

(defun set-opts (da node-idx options)
  (setf (aref (da-opts da) node-idx) options))

(defun set-chck (da base-idx arc &aux (next-idx (+ base-idx arc)))
  (setf (aref (da-chck da) next-idx) arc)
  next-idx)

(defun set-base (da node-idx base-idx)
  (setf (aref (da-base da) node-idx) base-idx))

;; XXX:
(defparameter *map* 
  (make-array #x10000 :initial-element nil))
(defparameter *cur* 0)

(defun build-impl (trie alloca da node-idx memo)
  (a.if #1=(gethash (trie::node-child trie) memo)
        (progn 
          (set-opts da node-idx (trie::node-options trie))
          (set-base da node-idx it))
    (let ((children (trie::collect-children trie)))
      (set-opts da node-idx (trie::node-options trie))
      (when children
        ;; XXX:
        (dolist (child children)
          (a.if #3=(aref *map* #2=(trie::node-label child))
              ()
            (setf (aref *map* #2#) #+IGNORE #2# #-IGNORE(incf *cur*))))

        (let ((base-idx (node-allocator:allocate 
                         alloca
                         #-IGNORE (mapcar (lambda (child) #3#) children)
                         #+IGNORE (mapcar #'trie::node-label children))))
          (setf #1# base-idx)
          (set-base da node-idx base-idx)
          (dolist (child children)
            (build-impl child alloca da
                        (set-chck da base-idx #3# #+IGNORE(trie::node-label child))
                        memo)))))))

(defun adjust (da)
  (with-slots (base chck opts) da
    (let ((max-base (loop FOR x ACROSS base MAXIMIZE x))
          (max-code (loop FOR x ACROSS chck UNLESS (= x #xFFFF) MAXIMIZE x)))
      (print `(:max ,max-base ,max-code))
      (setf base (subseq base 0 (+ max-base max-code 1))
            chck (subseq chck 0 (+ max-base max-code 1))
            opts (subseq opts 0 (+ max-base max-code 1)))))
  da)

(defun from-trie (trie output &aux (limit (node-count-limit trie)))
  ;; XXX:
  (defparameter *map* 
    (make-array #x10000 :initial-element nil))
  (defparameter *cur* 0)

  (let ((da (init-da limit)))
    (build-impl trie (node-allocator:make limit) da 0 
                (make-hash-table :test #'eq))
    (adjust da)
    (print `(:cur ,*cur*))
    (with-slots (base chck opts) da
      (with-open-file (out output :direction :output 
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
        (let ((node-count (node-count da)))
          (gomoku::write-int node-count out :width 4)
          
          (dotimes (i node-count)
            (let ((enc 0))
              (setf (ldb (byte 24  0) enc) (aref base i)
                    (ldb (byte 16 24) enc) (aref chck i)
                    (ldb (byte 24 40) enc) (aref opts i))
              (gomoku::write-int enc out :width 8)))
          #|
          (dotimes (i node-count)
            (gomoku::write-int (aref base i) out :width 4))
          (dotimes (i node-count)
            (gomoku::write-int (aref chck i) out :width 2))
          (dotimes (i node-count)
            (gomoku::write-int (aref opts i) out :width 4))
          |#
          ))))
  'done)