(when (> (length sb-ext:*posix-argv*) 2)
  (format *error-output* "Usage: sbcl --script make-build-dic-command.lisp [ouput-dir]~%")
  (sb-ext:quit :unix-status 1))

(require :asdf)
(asdf:load-system :gomoku)

(gomoku.util:defmain main (source-textdic-dir output-bindic-dir &optional (textdic-charset :euc-jp))
  "Usage: gomoku-build-dic <source-textdic-dir> <output-bindic-dir> [textdic-charset]"
  (let ((gomoku:*text-dictionary-charset* (intern (string-upcase textdic-charset) :keyword)))
    (gomoku:build-dic source-textdic-dir output-bindic-dir)))

(let ((path (merge-pathnames
             #P"gomoku-build-dic"
             (probe-file (or (second sb-ext:*posix-argv*)
                             *default-pathname-defaults*)))))
  (sb-ext:save-lisp-and-die path :toplevel #'main :executable t))
