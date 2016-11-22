(defun cider-cljs-repl (name)
  (caar (--filter (string-equal name (cadr it)) cider--cljs-repl-types)))
