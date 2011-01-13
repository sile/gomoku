(in-package :gomoku)

(declaim (inline write-int))

(defun write-int (int stream &key (width 1) (endian :big))
  (declare ((member :big :little) endian)
           ((member 1 2 4) width))
  (flet ((write-impl (pos) 
           (write-byte (ldb (byte 8 (* pos 8)) int) stream)))
    (declare (inline write-impl))
    (case endian
      (:big    (loop FOR i FROM (1- width) DOWNTO 0 DO (write-impl i)))
      (:little (loop FOR i FROM 0 BELOW width       DO (write-impl i))))))
