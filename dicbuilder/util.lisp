(in-package :gomoku.util)

(declaim (inline write-int))

(defun write-int (int stream &key (width 1) (endian :big))
  (declare ((member :big :little) endian)
           ((member 1 2 4 8) width))
  (flet ((write-impl (pos) 
           (write-byte (ldb (byte 8 (* pos 8)) int) stream)))
    (declare (inline write-impl))
    (case endian
      (:big    (loop FOR i FROM (1- width) DOWNTO 0 DO (write-impl i)))
      (:little (loop FOR i FROM 0 BELOW width       DO (write-impl i))))))

(defmacro package-alias (package &rest alias-list)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (rename-package ,package ,package ',alias-list)))

(defmacro with-time (msg &body body)
  `(let ((#1=#:beg (get-internal-real-time)))
     (format *error-output* "; ~A ... " ,msg)
     (force-output *error-output*)
     (prog1 (progn ,@body)
       (format *error-output* "~,3f sec~%" (/ (- (get-internal-real-time) #1#)
                                            INTERNAL-TIME-UNITS-PER-SECOND))
       (force-output *error-output*))))
     
(defmacro nlet (fn-name letargs &body body)
  `(labels ((,fn-name ,(mapcar #'car letargs)
              ,@body))
     (,fn-name ,@(mapcar #'cadr letargs))))

(defmacro each-line ((line filepath) &body body)
  `(with-open-file (#1=#:in ,filepath :external-format gomoku:*text-dictionary-charset*)
     (loop FOR ,line OF-TYPE (or null simple-string) = (read-line #1# nil nil)
           WHILE ,line
       DO
       (locally ,@body))))

(defmacro a.if (exp then else)
  `(let ((it ,exp))
     (if it
         ,then
       ,else)))

