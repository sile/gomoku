(in-package :gomoku)

(defun build-matrix (matrix.def matrix.bin)
  (declare #.*fastest*)
  (with-open-file (in matrix.def :external-format *text-dictionary-charset*)
    (with-open-file (out matrix.bin :direction :output :if-exists :supersede :element-type 'octet)
      (let ((left-num (read in))
            (right-num (read in)))
        (declare ((unsigned-byte 2) left-num right-num))
        (write-int left-num out :width 4)
        (write-int right-num out :width 4)

        (dotimes (l left-num)
          (dotimes (r right-num)
            #+IGNORE (assert (and (= l (read in)) (= r (read in))))
            (write-int (the (unsigned-byte 2) (read in)#|cost|#) out :width 2)))))))

(defun build-pos (id.def pos.bin)
  "POS is abbreviation of 'Parts Of Speech'"
  (with-open-file (out pos.bin :direction :output :if-exists :supersede)
    (each-line (line id.def)
      (let ((pos (subseq line (1+ (position #\Space line))
                         (position #\, line :from-end t))))
        (write-line pos out)))))

(defmacro each-char.def-line ((line char.def &key (type :code)) &body body)
  (declare ((member :category :code) type))
  `(each-line (,line ,char.def)
     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     (let* ((,line (string-trim '(#\Space #\Tab #\Return #\Newline) ,line))
            (,line (subseq ,line 0 (position #\# ,line)))
            (,line (string-trim '(#\Space #\Tab #\Return #\Newline) ,line)))
       (when (and (> (length ,line) 0)
                  (case ,type
                        (:category (char/= #\0 (char ,line 0)))
                        (:code     (char=  #\0 (char ,line 0)))))
         ,@body))))

(defmacro each-char-category (vars char.def &body body)
  `(each-char.def-line (#1=#:line ,char.def :type :category)
     (with-input-from-string (#2=#:in #1#)
       (multiple-value-bind ,vars (values #3=(read #2#) #3# #3# #3#)
         ,@body))))

(defun parse-char-category (char.def &aux categorys)
  (each-char-category (name) char.def 
    (push (intern (symbol-name name) :keyword) categorys))
  categorys)
  
(defun build-char-category (char.def category.bin)
  (with-open-file (out category.bin :direction :output :if-exists :supersede :element-type 'octet)
    (write-int (length (parse-char-category char.def)) out :width 4)
    (each-char-category (name invoke group length) char.def 
      (declare (ignore name))
      (write-int invoke out)
      (write-int group out)
      (write-int length out))))

(declaim (inline make-category))
(defun make-category (uses categorys)
  (let ((id (position (first uses) categorys)))
    (list id (loop FOR name IN uses
                   SUM (ash 1 (position name categorys))))))

(defun build-code-category (char.def code.bin)
  (let* ((categorys (parse-char-category char.def))
         (codes (make-array #x10000 :initial-element (make-category '(:default) categorys))))
    (each-char.def-line (line char.def :type :code)
      (let* ((mid (position #\Space line))
             (range (subseq line 0 mid))
             (category-spec (subseq line (1+ mid))))
        (let* ((beg (parse-integer range :start 2 :end 6 :radix 16))
               (end (if (= (length range) 6)
                        beg
                      (parse-integer range :start 10 :end 14 :radix 16)))
               (category (with-input-from-string (in category-spec)
                           (make-category (loop FOR c = (read in nil nil)
                                                WHILE c 
                                                COLLECT (intern (symbol-name c) :keyword))
                                          categorys))))
          (loop FOR code FROM beg TO end
                DO
                (setf (aref codes code) category)))))
    (with-open-file (out code.bin :direction :output :if-exists :supersede :element-type 'octet)
      (write-int (length codes) out :width 4)
      (loop FOR (category-id mask) ACROSS codes
        DO
        (write-int category-id out :width 1)
        (write-int mask out :width 2)))))

(defun collect-unk-morp (unk.def char.def)
  (let ((map (build-char-category char.def "/dev/null"))
        (morps (make-hash-table)))
    (each-line (line unk.def)
      (with-input-from-string (in2 (nsubstitute #\Space #\, line))
        (let ((word-id (position (read in2) map :test #'string=))
              (pos-id (read in2))
              (cost (progn (read in2) (read in2))))
          (push (list pos-id cost) (gethash word-id morps)))))
    morps))

(defun collect-morp (morps outdir &aux (da (double-array::load-dic 
                                            (merge-pathnames #P"word-id.bin" outdir))))
  (dolist (csv (directory #P"*.csv"))
    (each-line (line csv)
      (let* ((p1 (position #\, line))
             (p2 (position #\, line :start (1+ p1)))
             (p3 (position #\, line :start (1+ p2)))
             (p4 (position #\, line :start (1+ p3))))
        (let ((word-id (double-array::get-id (subseq line 0 p1) da))
              (pos-id (parse-integer line :start (1+ p1) :end p2))
              (cost (parse-integer line :start (1+ p3) :end p4)))
          ;;(assert word-id () "line:~A" line)
          ;; XXX: "￥"?
          (unless word-id
            (print line)
            (assert word-id))
          (when word-id
            (push (list pos-id cost) (gethash word-id morps)))))))
  morps)

(defun build-morp (unk.def char.def morp.bin)
  (let ((morps (collect-unk-morp unk.def char.def)))
    (collect-morp morps morp.bin)
    (print morps)
    ;; TODO:
    (let ((ms (make-array (+ (hash-table-count morps) 10) #|XXX: 10+?|# :initial-element '())))
      (maphash (lambda (word-id vs)
                 (setf (aref ms word-id) vs))
               morps)
      (with-open-file (out morp.bin :direction :output :if-exists :supersede
                           :element-type '(unsigned-byte 8))
        (loop FOR vs ACROSS ms
          DO
          (loop FOR (pos-id cost) IN vs
            DO
            (write-int pos-id out :width 2)
            (write-int cost out :width 2))))
      
      (with-open-file (out (merge-pathnames "id-morps.bin" morp.bin)
                           :direction :output :if-exists :supersede
                           :element-type '(unsigned-byte 8))
        (loop WITH offset = 0
              FOR vs ACROSS ms
          DO
          (write-int offset out :width 4)
          (incf offset (length vs))
          FINALLY
          (write-int offset out :width 4)))
       )))

(defun build-dic (text-dic-dir output-dir)
  (ensure-directories-exist output-dir)
  (let ((output-dir (probe-file output-dir))
        (*default-pathname-defaults* (probe-file text-dic-dir)))
    (flet ((out-path (filename)
             (merge-pathnames filename output-dir)))
      #+IGNORE
      (with-time "matrix.bin" 
                 (build-matrix #P"matrix.def" (out-path #P"matrix.bin")))
      (with-time "pos.bin" 
                 (build-pos #P"left-id.def" (out-path #P"pos.bin")))
      (with-time "category.bin" 
                 (build-char-category #P"char.def" (out-path #P"category.bin")))
      (with-time "code.bin"
                 (build-code-category #P"char.def" (out-path #P"code.bin")))
      (with-time "surface-id.bin"
                 (double-array:build *default-pathname-defaults* output-dir))
      
      (format *error-output* "; morp.bin~%")
      (build-morp #P"unk.def" #P"char.def" (merge-pathnames "morp.bin" output-dir))
    'done)))
