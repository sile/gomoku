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

(defun build-dic (text-dic-dir output-dir)
  (let ((*default-pathname-defaults* (probe-file text-dic-dir)))
    (build-matrix #P"matrix.def" (concatenate 'string  output-dir"matrix.bin"))
    (build-pos-data #P"left-id.def" (concatenate 'string output-dir"pos.bin"))
    (build-char-category #P"char.def" (concatenate 'string output-dir"category.bin"))
    (build-code-category #P"char.def" (concatenate 'string output-dir"code.bin")))
  'done)