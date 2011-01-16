(defpackage gomoku
  (:use :common-lisp)
  (:export :build-dic))
(in-package :gomoku)

(deftype octet () '(unsigned-byte 8))
(defvar *text-dictionary-charset* :euc-jp)

