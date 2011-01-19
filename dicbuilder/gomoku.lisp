(in-package :gomoku)

(defun build-matrix (matrix.def matrix.bin)
  (with-open-file (in matrix.def :external-format *text-dictionary-charset*)
    (with-open-file (out matrix.bin :direction :output :if-exists :supersede :element-type 'octet)
      (let ((left-num (read in))
            (right-num (read in)))
        (write-int left-num out :width 4)
        (write-int right-num out :width 4)

        (dotimes (l left-num)
          (dotimes (r right-num)
            (assert (and (= l (read in)) (= r (read in))))
            (write-int (read in)#|cost|# out :width 2))))))
  'done)

(defun build-pos-data (id.def pos.bin)
  "'pos' is abbreviation of 'Part Of Speechs"
  (with-open-file (in id.def :external-format *text-dictionary-charset*)
    (with-open-file (out pos.bin :direction :output :if-exists :supersede)
      (loop FOR line = (read-line in nil nil)
            WHILE line
        DO
        (let ((pos (subseq line (1+ (position #\Space line))
                                (position #\, line :from-end t))))
          (write-line pos out)))))
  'done)

(defmacro each-char.def-line ((line char.def &key (type :code)) &body body)
  (declare ((member :category :code) type))
  (let ((in (gensym)))
    `(with-open-file (,in ,char.def :external-format *text-dictionary-charset*)
       (loop FOR ,line = (read-line ,in nil nil)
             WHILE ,line
         DO
         (let* ((,line (string-trim '(#\Space #\Tab #\Return #\Newline) ,line))
                (,line (subseq ,line 0 (position #\# ,line)))
                (,line (string-trim '(#\Space #\Tab #\Return #\Newline) ,line)))
           (when (and (> (length ,line) 0)
                      (case ,type
                        (:category (char/= #\0 (char ,line 0)))
                        (:code     (char=  #\0 (char ,line 0)))))
             ,@body))))))

(defun build-char-category (char.def category.bin &aux categorys)
  (with-open-file (out category.bin :direction :output :if-exists :supersede :element-type 'octet)
    (each-char.def-line (line char.def :type :category)
      (with-input-from-string (in line)
        (multiple-value-bind (name invoke group length)
                             (values #1=(read in) #1# #1# #1#)
          (push name categorys)
          (write-int invoke out)
          (write-int group out)
          (write-int length out)))))
  (mapcar (lambda (x) (intern (symbol-name x) :keyword)) (nreverse categorys)))

(defun make-category (categorys category-id-map)
  (let ((id (position (car categorys) category-id-map)))
    (list id (loop FOR name IN category-id-map
                   SUM (ash 1 (position name category-id-map))))))

(defun build-code-category (char.def code.bin)
  (let* ((map (build-char-category char.def "/dev/null"))
         (codes (make-array #x10000 :initial-element (make-category '(:default) map))))
    (each-char.def-line (line char.def :type :code)
      (let* ((mid (position #\Space line))
             (range (subseq line 0 mid))
             (categorys (subseq line (1+ mid))))
        (let* ((beg (parse-integer range :start 2 :end 6 :radix 16))
               (end (if (= (length range) 6)
                        beg
                      (parse-integer range :start 10 :end 14 :radix 16)))
               (category (with-input-from-string (in categorys)
                           (make-category (loop FOR c = (read in nil nil)
                                                WHILE c 
                                                COLLECT (intern (symbol-name c) :keyword))
                                          map))))
          (loop FOR code FROM beg TO end
                DO
                (setf (aref codes code) category)))))
    (with-open-file (out code.bin :direction :output :if-exists :supersede :element-type 'octet)
      (loop FOR (category-id mask) ACROSS codes
        DO
        (write-int category-id out :width 1)
        (write-int mask out :width 2))))
  'done)

(defun collect-unk-morp (unk.def char.def)
  (let ((map (build-char-category char.def "/dev/null"))
        (morps (make-hash-table)))
    (with-open-file (in unk.def :external-format *text-dictionary-charset*)
      (loop FOR line = (read-line in nil nil)
            WHILE line
        DO
        (with-input-from-string (in2 (nsubstitute #\Space #\, line))
          (let ((word-id (position (read in2) map :test #'string=))
                (pos-id (read in2))
                (cost (progn (read in2) (read in2))))
            (push (list pos-id cost) (gethash word-id morps))))))
    morps))

(defun collect-morp (morps outdir &aux (da (double-array::load-dic 
                                            (merge-pathnames #P"word-id.bin" outdir))))
  (dolist (csv (directory #P"*.csv"))
    (with-open-file (in csv :external-format *text-dictionary-charset*)
      (loop FOR line = (read-line in nil nil)
            WHILE line
        DO
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
              (print line))
            (when word-id
              (push (list pos-id cost) (gethash word-id morps))))))))
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

(defun build-dic (text-dic-dir output-dir &aux (output-dir (probe-file output-dir)))
  (let ((*default-pathname-defaults* (probe-file text-dic-dir)))
    (format *error-output* "; word-id.bin~%")
    (double-array::from-trie (trie::build-from-file #P"ipa.word") (merge-pathnames "word-id.bin" output-dir))
    (format *error-output* "; morp.bin~%")
    (build-morp #P"unk.def" #P"char.def" (merge-pathnames "morp.bin" output-dir))
    (format *error-output* "; matrix.bin~%")
    (build-matrix #P"matrix.def" (merge-pathnames "matrix.bin" output-dir))
    (format *error-output* "; pos.bin~%")
    (build-pos-data #P"left-id.def" (merge-pathnames "pos.bin" output-dir))
    (format *error-output* "; category.bin~%")
    (print (build-char-category #P"char.def" (merge-pathnames "category.bin" output-dir)))
    (format *error-output* "; code.bin~%")
    (build-code-category #P"char.def" (merge-pathnames "code.bin" output-dir)))
  'done)
