(defpackage double-array
  (:use :common-lisp :gomoku)
  (:export from-trie))
(in-package :double-array)

(defstruct da
  (base #() :type (simple-array (unsigned-byte 32)))
  (chck #() :type (simple-array (unsigned-byte 16)))
  (opts #() :type (simple-array (unsigned-byte 32))))

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