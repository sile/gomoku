(defpackage gomoku.util
  (:use :common-lisp)
  (:export write-int
           with-time
           package-alias
           nlet
           each-line
           a.if
           it))

(defpackage gomoku
  (:use :common-lisp :gomoku.util)
  (:export build-dic
           *text-dictionary-charset*
           done))
(in-package :gomoku)

(deftype octet () '(unsigned-byte 8))
(defvar *text-dictionary-charset* :euc-jp)

(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0)))

