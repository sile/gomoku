(defpackage double-array
  (:use :common-lisp :gomoku.util)
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
(defvar *map* )
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
    ;;(print `(:cur ,*cur*))
    (with-slots (base chck opts) da
      (with-open-file (out output :direction :output 
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
        (let ((node-count (node-count da)))
          (gomoku::write-int node-count out :width 4)
          
          (dotimes (i node-count)
            (when (< i 10)
              (format t "BASE:~4,'0x CHCK:~2,'0x OPTS:~4,'0x~%" 
                      (aref base i) (aref chck i) (aref opts i)))
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

  ;; XXX:
  (with-open-file (out (merge-pathnames #P"code-map.bin" output)
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (loop FOR c ACROSS *map*
      DO
      (gomoku::write-int (or c 0) out :width 2)))
  'done)

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
  (nodes #() :type (simple-array (unsigned-byte 64))))

(defun read-int (in &key (width 1))
  (loop FOR i FROM (1- width) DOWNTO 0
        SUM (ash (read-byte in) (* i 8))))

(defun load-dic (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let* ((node-count (read-int in :width 4))
           (nodes (make-array node-count :element-type '(unsigned-byte 64))))
      (dotimes (i node-count)
        (setf (aref nodes i) (read-int in :width 8)))

      (make-dawg :nodes nodes))))

(defmacro nlet (fn-name letargs &body body)
  `(labels ((,fn-name ,(mapcar #'car letargs)
              ,@body))
     (,fn-name ,@(mapcar #'cadr letargs))))

(defun get-id (key dawg &key (start 0) (end (length key)))
  (with-slots (nodes) (the dawg dawg)
    (let ((in (code-stream:make key :start start :end end)))
      (nlet recur ((node 0) (id -1)) ; TODO: idには未知語分のオフセットを加える
        (declare (fixnum id))
        ;;(print `(:node ,node :id ,id))
        (if (code-stream:eos? in)
            (and (terminal? nodes node) (inc-id id nodes node))
          (let* ((arc (or (aref *map* (code-stream:read in)) 0))  ;; XXX:
                 (next (+ (base nodes node) arc)))
            ;;(print `(:arc ,arc :next ,next :chck ,(chck nodes next)))
            (when (= (chck nodes next) arc)
              (recur next (inc-id id nodes node)))))))))
