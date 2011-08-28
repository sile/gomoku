(in-package :gomoku.util)

(declaim (inline write-int))

(defun write-int (int stream &key (width 1))
  (declare ((member 1 2 4 8) width))
  (flet ((write-impl (pos) 
           (write-byte (ldb (byte 8 (* pos 8)) int) stream)))
    (declare (inline write-impl))
    (loop FOR i FROM (1- width) DOWNTO 0 DO (write-impl i))))

(defmacro package-alias (package &rest alias-list)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (rename-package ,package ,package ',alias-list)))

(defmacro with-time (msg &body body)
  `(let ((#1=#:beg (get-internal-real-time)))
     (format *error-output* ";  ~A ... " ,msg)
     (force-output *error-output*)
     (prog1 (progn ,@body)
       (format *error-output* "~,3f sec~%" (/ (- (get-internal-real-time) #1#)
                                            INTERNAL-TIME-UNITS-PER-SECOND))
       (force-output *error-output*))))
     
(defmacro each-line ((line filepath) &body body)
  `(with-open-file (#1=#:in ,filepath :external-format gomoku:*text-dictionary-charset*)
     (loop FOR ,line OF-TYPE (or null simple-string) = (read-line #1# nil nil)
           WHILE ,line
       DO
       (locally ,@body))))

(defun basename (pathstring)
    (let ((path (parse-namestring pathstring)))
      (format nil "~A~@[.~A~]" (pathname-name path) (pathname-type path))))

(defun collect-varsym (args)
    (mapcar (lambda (a)
	      (if (consp a) (car a) a))
	    (remove-if (lambda (a)
			 (and (symbolp a) (string= "&" a :end2 1)))
		       args)))

(defmacro defmain (fn-name args &body body)
  (let ((usage nil))
    ;; If first expression of body is string type, it treated as command documentation
    (when (stringp (car body))
      (setf usage (car body)
	    body  (cdr body)))
    
    `(defun ,fn-name ()
       ;; Need to override *invoke-debugger-hook*
       (let ((sb-ext:*invoke-debugger-hook*
	      (lambda (condition hook)
		(declare (ignore hook))
		(format *error-output* "Error: ~A~%" condition)
		(sb-ext:quit :unix-status 1))))
         
	 ;; When failed arguments destructuring, show documentation and exit
	 ,(when usage
	    `(handler-case 
	      (destructuring-bind ,args (cdr sb-ext:*posix-argv*) 
	        (declare (ignore ,@(collect-varsym args))))
	      (error ()
	        (format *error-output* "~&~?~%~%" 
			,usage
			(list (basename (car sb-ext:*posix-argv*))))
		(sb-ext:quit :unix-status 1))))

         (destructuring-bind ,args (cdr sb-ext:*posix-argv*)
           ,@body
	   (sb-ext:quit :unix-status 0))))))
