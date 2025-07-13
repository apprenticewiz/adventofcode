#!/usr/bin/sbcl --script

(defun usage ()
  (format *error-output* "usage: main.lisp <input file>~%")
  (quit :unix-status 1))

(defun make-key (pos)
  (format nil "~D,~D" (car pos) (cadr pos)))

(defun move (pos dir)
  (destructuring-bind (x y) pos
    (case dir
      (#\^ (list x (+ y 1)))
      (#\v (list x (- y 1)))
      (#\< (list (- x 1) y))
      (#\> (list (+ x 1) y))
      (t pos))))

(defun process (filename)
  (with-open-file (infile filename :direction :input)
    (let ((positions (make-hash-table :test 'equal))
          (santa '(0 0))
          (robo-santa '(0 0))
          (move-santa t))
      (setf (gethash (make-key santa) positions) t)
      (loop for line = (read-line infile nil)
            while line
            do (loop for ch across line
                     do (if move-santa
                          (progn
                            (setf santa (move santa ch))
                            (setf (gethash (make-key santa) positions) t))
                          (progn
                            (setf robo-santa (move robo-santa ch))
                            (setf (gethash (make-key robo-santa) positions) t)))
                        (setf move-santa (not move-santa))))
      (hash-table-count positions))))

(defun main ()
  (let ((args (cdr *posix-argv*)))
    (if (= (length args) 1)
        (let* ((filename (first args))
               (result (process filename)))
          (format t "result = ~A~%" result))
        (usage))))

(main)
