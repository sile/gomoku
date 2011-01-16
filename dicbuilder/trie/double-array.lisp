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
    (round (* 2.5 (hash-table-count map)))))

(defun init-da (node-count-limit)
  (make-da :base (make-array node-count-limit :element-type '(unsigned-byte 32) :initial-element 0)
           :chck (make-array node-count-limit :element-type '(unsigned-byte 16) :initial-element 0)
           :opts (make-array node-count-limit :element-type '(unsigned-byte 32) :initial-element 0)))

(defun set-opts (da node-idx options)
  (setf (aref (da-opts da) node-idx) options))

(defun set-chck (da base-idx arc &aux (next-idx (+ base-idx arc)))
  (setf (aref (da-chck da) next-idx) arc)
  next-idx)

(defun set-base (da node-idx base-idx)
  (setf (aref (da-base da) node-idx) base-idx))

(defun build-impl (trie alloca da node-idx memo)
  (a.if #1=(gethash (trie::node-child trie) memo)
        (progn 
          (set-opts da node-idx (trie::node-options trie))
          (set-base da node-idx it))
    (let ((children (trie::collect-children trie)))
      (set-opts da node-idx (trie::node-options trie))
      (when children
        (let ((base-idx (node-allocator:allocate 
                         alloca
                         (mapcar #'trie::node-label children))))
          (setf #1# base-idx)
          (set-base da node-idx base-idx)
          (dolist (child children)
            (build-impl child alloca da
                        (set-chck da base-idx (trie::node-label child))
                        memo)))))))

(defun from-trie (trie output)
  (let ((da (init-da (node-count-limit trie))))
    ;; build-impl
    (with-slots (base chck opts) da
      (with-open-file (out output :direction :output 
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
        (let ((node-count (node-count da)))
          (gomoku::write-int node-count out :width 4)
          (dotimes (i node-count)
            (gomoku::write-int (aref base i) out :width 4))
          (dotimes (i node-count)
            (gomoku::write-int (aref chck i) out :width 2))
          (dotimes (i node-count)
            (gomoku::write-int (aref opts i) out :width 4))))))
  'done)