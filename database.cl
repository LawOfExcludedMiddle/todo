(defvar *task-list* nil)

(defun prompt (text)
    (format *query-io* "~a" text)
    (force-output *query-io*)
    (read-line *query-io*))

(defun add-task (name desc)
    (push (list name desc) *task-list*)
    (save))

(defun remove-task (task) 
    (setf *task-list* (append (cdr (member task (reverse *task-list*) :test #'equal)) (cdr (member task *task-list* :test #'equal))))
    (save)
    (print-tasks))

(defun print-tasks ()
    (setq x 1)
    (format t "TODO:~%-----~%")
    (dolist (item *task-list*)
        (format t "~a: " x)
        (incf x)
        (format t "~{~a~%~}" (cdr (reverse item))))
    (setq x (handler-case (parse-integer (prompt "> ")) (error (e) (sb-ext:exit))))
    (if (= 0 x) (new-task) (read-task x)))

(defun task (num)
    (setq l *task-list*)
    (dotimes (n (- num 1)) 
        (setq l (cdr l)))
    (car l))

(defun read-task (num) 
    (if (= 0 (length *task-list*)) (print-tasks) (format t "Task name: ~{~a~%Description: ~a~}~%" (task num)))
    (if (y-or-n-p "Delete task?") (remove-task (task num)) (print-tasks)))

(defun new-task () 
    (add-task (prompt "Task name: ") (prompt "Task description: "))
    (print-tasks))

(defun save ()
    (with-open-file (out "tasks.christo" :direction :output :if-exists :supersede)
        (with-standard-io-syntax (print *task-list* out))))

(defun reload ()
    (with-open-file (in "tasks.christo") 
        (with-standard-io-syntax (setf *task-list* (read in)))))

(defun main ()
    (handler-case (reload) (error (e) ()))
    (print-tasks))
