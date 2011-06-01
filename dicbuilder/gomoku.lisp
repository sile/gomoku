(in-package :gomoku)

(package-alias :gomoku.trie :trie)
(package-alias :gomoku.trie.double-array :double-array)

(defun build-matrix (matrix.def matrix.bin)
  (declare #.*fastest*)
  (with-open-file (in matrix.def :external-format *text-dictionary-charset*)
    (with-open-file (out matrix.bin :direction :output :if-exists :supersede :element-type 'octet)
      (let ((left-num (read in))
            (right-num (read in)))
        (declare ((unsigned-byte 16) left-num right-num))
        (write-int left-num out :width 4)
        (write-int right-num out :width 4)

        (dotimes (l left-num)
          (dotimes (r right-num)
            (assert (and (= l (read in)) (= r (read in))))
            (write-int (the (unsigned-byte 16) (read in)#|cost|#) out :width 2)))))))

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
  (nreverse categorys))
  
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

(defmacro each-morpheme ((surface pos-id cost) filepath &body body)
  `(each-line (#0=#:line ,filepath) 
     (let* ((#1=#:p1 (position #\, #0#))
            (#2=#:p2 (position #\, #0# :start (1+ #1#)))
            (#3=#:p3 (position #\, #0# :start (1+ #2#)))
            (#4=#:p4 (position #\, #0# :start (1+ #3#))))
       (let ((,surface (subseq #0# 0 #1#))
             (,pos-id  (parse-integer #0# :start (1+ #1#) :end #2#))
             (,cost    (parse-integer #0# :start (1+ #3#) :end #4#)))
         ,@body))))

(defun collect-unk-morp (unk.def)
  (let ((categorys (parse-char-category #P"char.def"))
        (morps (make-hash-table)))
    (each-morpheme (category-name pos-id cost) unk.def
      (let ((morp-id (position category-name categorys :test #'string=)))
        (push (list pos-id cost) (gethash morp-id morps))))
    morps))

(defun collect-morp (morps outdir &aux (da (double-array:load-dic outdir)))
  (let ((offset (length (parse-char-category #P"char.def"))))
    (dolist (csv (directory #P"*.csv"))
      (each-morpheme (surface pos-id cost) csv
        (let ((morp-id (double-array:get-id surface da)))
          (assert morp-id)
          (push (list pos-id cost) (gethash (+ offset morp-id) morps))))))
  morps)

;; XXX: ambiguous name
(defun delete-unused-morp (morps)
  (flet ((morp< (a b)
           (if (< (first a) (first b))
               t
             (if (> (first a) (first b))
                 nil
               (< (second a) (second b))))))
    (delete-duplicates (sort morps #'morp<) :key #'first :from-end t)))

(defun build-morp (unk.def morp.bin)
  (let ((morps (collect-unk-morp unk.def)))
    (collect-morp morps (directory-namestring morp.bin))

    (let ((ms (make-array (hash-table-count morps) :initial-element '())))
      (maphash (lambda (morp-id vs)
                 (setf (aref ms morp-id) (delete-unused-morp vs)))
               morps)
      (with-open-file (out morp.bin :direction :output :if-exists :supersede
                                    :element-type 'octet)
        (write-int (length ms) out :width 4)
        (loop FOR vs ACROSS ms
          DO
          (loop FOR (pos-id cost) IN vs
            DO
            (write-int pos-id out :width 2)
            (write-int cost out :width 2))))
      
      (with-open-file (out (merge-pathnames "id-morphemes-map.bin" morp.bin)
                           :direction :output :if-exists :supersede
                           :element-type 'octet)
        (write-int (length ms) out :width 4)
        (loop FOR vs ACROSS ms
          DO
          (assert (< (length vs) #x80))
          (write-int (length vs) out))))))

(defun build-dic (text-dic-dir output-dir)
  (format *error-output* "; = BUILD DICTIONARY =~%")
  (format *error-output* "; source text dictionary:   ~A~%" text-dic-dir)
  (format *error-output* "; output binary dictionary: ~A~%" output-dir)

  (ensure-directories-exist output-dir)
  (format *error-output* ";~%; build:~%")
  (let ((output-dir (probe-file output-dir))
        (*default-pathname-defaults* (probe-file text-dic-dir)))
    (flet ((out-path (filename)
             (merge-pathnames filename output-dir)))
      (with-time "matrix" 
                 (build-matrix #P"matrix.def" (out-path #P"matrix.bin")))
      (with-time "parts-of-speech" 
                 (build-pos #P"left-id.def" (out-path #P"pos.bin")))
      (with-time "char-category" 
                 (build-char-category #P"char.def" (out-path #P"category.bin")))
      (with-time "code-category"
                 (build-code-category #P"char.def" (out-path #P"code.bin")))
      (with-time "surface-id"
                 (double-array:build *default-pathname-defaults* output-dir))
      (with-time "morpheme"
        (build-morp #P"unk.def" (out-path "morpheme.bin")))))
  (format *error-output* ";~%; done~%")
  'done)

(package-alias :gomoku.trie)
(package-alias :gomoku.trie.double-array)
