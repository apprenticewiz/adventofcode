#!/usr/bin/sbcl --script

(defun usage ()
  (format *error-output* "usage: main.lisp <input file>~%")
  (quit :unix-status 1))

(defun make-key (pos)
  (format nil "~D,~D" (car pos) (cadr pos)))

(defun prop1 (line)
  (loop for i from 0 to (- (length line) 2)
        for pair = (subseq line i (+ i 2))
        thereis (loop for j from (+ i 2) to (- (length line) 2)
                      thereis (string= pair (subseq line j (+ j 2))))))

(defun prop2 (line)
  (loop for i from 0 below (- (length line) 2)
        thereis (char= (char line i) (char line (+ i 2)))
  ))

(defun process (filename)
  (with-open-file (infile filename :direction :input)
    (let ((n 0))
      (loop for line = (read-line infile nil)
            while line
            do (when (and (prop1 line) (prop2 line))
                  (incf n)))
      n)))

(defun main ()
  (let ((args (cdr *posix-argv*)))
    (if (= (length args) 1)
        (let* ((filename (first args))
               (result (process filename)))
          (format t "result = ~A~%" result))
        (usage))))

(main)
