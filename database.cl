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
    (cls)
    (print-tasks))

(defun cls ()
    (format t "~A[H~@*~A[J" #\escape))

(defun print-tasks ()
    (setq x 1)
    (format t "TODO:~%-----~%")
    (dolist (item *task-list*)
        (format t "~a: " x)
        (incf x)
        (format t "~{~a~%~}" (cdr (reverse item))))
    (setq x (prompt "lance> "))
    (handler-case (setq y (parse-integer x)) (error (e) (setq y (- 0 1)))) 
    (if (= (- 0 1) y) (parse-str x) (parse-int y)))

(defun del-ambiguous ()
    (remove-task (task (parse-integer (prompt "Enter task to delete: ")))))

(defun parse-str (x)
    (handler-case (if (string-equal (subseq x 0 3) "del") (if (= 3 (length x)) (del-ambiguous) (remove-task (task (parse-integer (subseq x 3))))) (if (string-equal (subseq x 0 3) "add") (new-task) (if (string-equal (subseq x 0 4) "exit") (sb-ext:quit)))) (error (e) (learn-to-type))))

(defun learn-to-type ()
    (cls)
        (format t "Learn to type, baka.~%")
    (print-tasks))

(defun parse-int (x)
    (if (<= x 0) (not-num) (read-task x)))

(defun not-num ()
    (format t "Choose a better number, baka.~%")
    (cls)
    (print-tasks))

(defun task (num)
    (setq l *task-list*)
    (dotimes (n (- num 1)) 
        (setq l (cdr l)))
    (car l))

(defun read-task (num) 
    (if (= 0 (length *task-list*)) (print-tasks) (format t "Task name: ~{~a~%Description: ~a~}~%" (task num)))
    (prompt "PRESS ENTER TO CONTINUE . . . ")
    (cls)
    (print-tasks)) 

(defun new-task () 
    (add-task (prompt "Task name: ") (prompt "Task description: "))
    (cls)
    (print-tasks))

(defun save ()
    (with-open-file (out ".tasks.christo" :direction :output :if-exists :supersede)
        (with-standard-io-syntax (print *task-list* out))))

(defun reload ()
    (with-open-file (in ".tasks.christo") 
        (with-standard-io-syntax (setf *task-list* (read in)))))

(defun main ()
    (handler-case (reload) (error (e) ()))
    (cls)
    (
print-tasks))
