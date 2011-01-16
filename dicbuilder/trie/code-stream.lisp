;;;; TODO: サロゲートペア対応
(defpackage code-stream
  (:use :common-lisp :gomoku)
  (:shadow :common-lisp read peek)
  (:export code-stream
           make
           eos?
           peek
           eat
           read

           init-code-mapping
           save-code-mapping))
(in-package :code-stream)

(declaim (inline make-code-stream make eos? peek eat read))

(defvar *map*)

(defstruct code-stream
  (src 0 :type simple-string)
  (pos 0 :type fixnum)
  (end 0 :type fixnum))

(defun make (string &key (start 0) (end (length string)))
  (make-code-stream :src string :pos start :end end))

(defun eos? (in)
  (with-slots (pos end) (the code-stream in)
    (= pos end)))

(defun peek (in)
  (with-slots (src pos) (the code-stream in)
    (aref *map* (char-code (char src pos)))))

(defun eat (in)
  (with-slots (pos) (the code-stream in)
    (unless (eos? in)
      (incf pos)))
  in)

(defun read (in)
  (prog1 (peek in)
    (eat in)))

(defun init-code-mapping (source-file)
  (let ((map (make-array #x10000 :initial-element -1)))
    (with-open-file (in source-file)
      (loop FOR line = (read-line in nil nil)
            WHILE line
        DO
        (loop FOR c ACROSS line
          DO
          (setf (aref map (char-code c)) 0))))
    
    (loop WITH cd = -1
          FOR i FROM 0 BELOW (length map)
          WHEN (zerop (aref map i))
      DO
      (setf (aref map i) (incf cd)))
    (defparameter *map* map))
  'done)

(defun save-code-mapping (output-file)
  (with-open-file (out output-file :direction :output 
                       :if-exists :supersede :element-type '(unsigned-byte 8))
    (loop FOR cd ACROSS *map*
      DO
      (gomoku::write-int cd out :width 2)))
  'done)
