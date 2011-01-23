(defpackage double-array
  (:use :common-lisp :gomoku.util)
  (:export build
           load-dic
           get-id))
(in-package :double-array)

(deftype uint1 () '(unsigned-byte 8))
(deftype uint2 () '(unsigned-byte 16))
(deftype uint3 () '(unsigned-byte 24))
(deftype uint4 () '(unsigned-byte 32))
(deftype uint8 () '(unsigned-byte 64))

(defstruct da
  (base #() :type (simple-array uint4))
  (chck #() :type (simple-array uint2))
  (opts #() :type (simple-array uint4)))

(defun node-count (da)
  (length (da-base da)))

(defun node-count-limit (trie &aux (set (make-hash-table :test #'eq)))
  (declare #.gomoku::*fastest*)
  (nlet recur ((node trie))
        (unless (gethash node set)
          (setf (gethash node set) t)
          (dolist (child (trie:collect-children node))
            (recur child))))
  (the fixnum (* (hash-table-count set) 4)))

(defun init-da (node-count-limit)
  (make-da :base (make-array node-count-limit :element-type 'uint4 :initial-element 0)
           :chck (make-array node-count-limit :element-type 'uint2 :initial-element #xFFFF)
           :opts (make-array node-count-limit :element-type 'uint4 :initial-element 0)))

(declaim (inline set-opts set-chck set-base))
(defun set-opts (da node-idx options)
  (setf (aref (da-opts da) node-idx) options))

(defun set-chck (da base-idx arc &aux (next-idx (the uint3 (+ base-idx arc))))
  (setf (aref (da-chck da) next-idx) arc)
  next-idx)

(defun set-base (da node-idx base-idx)
  (setf (aref (da-base da) node-idx) base-idx))

(defun build-impl (trie alloca da node-idx memo code-map cur-code)
  (declare #.gomoku::*fastest*
           ((simple-array uint2) code-map)
           (uint2 cur-code))
  (a.if #1=(gethash (trie::node-child trie) memo)
        (progn 
          (set-opts da node-idx (trie:node-options trie))
          (set-base da node-idx it))
    (let ((children (trie:collect-children trie)))
      (flet ((get-code (child)
               (when (= 0 #2=(aref code-map (trie::node-label child)))
                 (setf #2# (incf cur-code)))
               (the uint2 #2#)))
        (declare (inline get-code))
        (set-opts da node-idx (trie:node-options trie))
        (when children
          (let ((base-idx (node-allocator:allocate 
                           alloca
                           (mapcar #'get-code children))))
            (declare (uint3 base-idx))
            (setf #1# base-idx)
            (set-base da node-idx base-idx)
            (dolist (child children)
              (setf cur-code 
                    (build-impl child alloca da
                                (set-chck da base-idx (get-code child))
                                memo code-map cur-code))))))))
  cur-code)

(defun adjust (da)
  (with-slots (base chck opts) da
    (let ((max-base (loop FOR x ACROSS base MAXIMIZE x))
          (max-code (loop FOR x ACROSS chck UNLESS (= x #xFFFF) MAXIMIZE x)))
      (setf base (subseq base 0 (+ max-base max-code 1))
            chck (subseq chck 0 (+ max-base max-code 1))
            opts (subseq opts 0 (+ max-base max-code 1)))))
  da)

(defun build-from-trie (trie output-dir &aux (limit (node-count-limit trie)))
  (let ((da (init-da limit))
        (code-map (make-array #x10000 :initial-element 0 :element-type 'uint2))
        (cur-code 0)
        (*default-pathname-defaults* (probe-file output-dir)))

    (build-impl trie (node-allocator:make limit) da 0 
                (make-hash-table :test #'eq) code-map cur-code)
    (adjust da)

    (with-slots (base chck opts) da
      (with-open-file (out #P"surface-id.bin" :direction :output 
                                              :if-exists :supersede
                                              :element-type 'uint1)
        (let ((node-count (node-count da)))
          (write-int (node-count da) out :width 4)
          
          (dotimes (i node-count)
            (let ((enc 0))
              (setf (ldb (byte 24  0) enc) (aref base i)
                    (ldb (byte 16 24) enc) (aref chck i)
                    (ldb (byte 24 40) enc) (aref opts i))
              (write-int enc out :width 8))))))

    (with-open-file (out #P"code-map.bin" :direction :output
                                          :if-exists :supersede
                                          :element-type 'uint1)
      (write-int (length code-map) out :width 4)
      (loop FOR c ACROSS code-map
            DO
            (gomoku::write-int c out :width 2)))))

(defun build (text-dic-dir output-dir)
  (build-from-trie (trie:build text-dic-dir) output-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function(2)
(declaim (inline base chck terminal? sibling-total inc-id))
(defun base (nodes idx)
  (ldb (byte 24 0) (aref nodes idx)))

(defun chck (nodes idx)
  (ldb (byte 16 24) (aref nodes idx)))

(defun terminal? (nodes idx)
  (ldb-test (byte 1 40) (aref nodes idx)))

(defun sibling-total (nodes idx)
  (ldb (byte 23 41) (aref nodes idx)))

(defun inc-id (id nodes node)
  (+ id (if (terminal? nodes node) 1 0) (sibling-total nodes node)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; external function(2)
(defstruct dawg
  (code-map #() :type (simple-array uint2))
  (nodes    #() :type (simple-array uint8)))

(defun load-dic (dic-dir &aux (*default-pathname-defaults* (probe-file dic-dir)))
  (flet ((load-nodes ()
           (with-open-file (in #P"surface-id.bin" :element-type 'uint1)
             (let* ((node-count (read-int in :width 4))
                    (nodes (make-array node-count :element-type 'uint8)))
               (dotimes (i node-count nodes)
                 (setf (aref nodes i) (read-int in :width 8))))))
         (load-code-map ()
           (with-open-file (in #P"code-map.bin" :element-type 'uint1)
             (let* ((code-limit (read-int in :width 4))
                    (code-map (make-array code-limit :element-type 'uint2)))
               (dotimes (i code-limit code-map)
                 (setf (aref code-map i) (read-int in :width 2)))))))
    (make-dawg :nodes (load-nodes)
               :code-map (load-code-map))))

(defun get-id (key dawg &key (start 0) (end (length key)))
  (declare (simple-string key))
  (with-slots (nodes code-map) (the dawg dawg)
    (let ((in (code-stream:make key :start start :end end)))
      (nlet recur ((node 0) (id -1))
        (declare (fixnum id node))
        (if (code-stream:eos? in)
            (and (terminal? nodes node) (inc-id id nodes node))
          (let* ((arc (aref code-map (code-stream:read in)))
                 (next (+ (base nodes node) arc)))
            (when (= (chck nodes next) arc)
              (recur next (inc-id id nodes node)))))))))
