(defun main ()
    (load "database.cl")
    (sb-ext:save-lisp-and-die "todo" :executable t :toplevel 'main))
