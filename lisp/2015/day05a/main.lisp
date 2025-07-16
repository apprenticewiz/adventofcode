#!/usr/bin/sbcl --script

(defun usage ()
  (format *error-output* "usage: main.lisp <input file>~%")
  (quit :unix-status 1))

(defun make-key (pos)
  (format nil "~D,~D" (car pos) (cadr pos)))

(defun prop1 (line)
  (let ((num-vowels (count-if (lambda (ch) (find ch "aeiou")) line)))
    (>= num-vowels 3)))

(defun prop2 (line)
  (loop for i from 1 below (length line)
        thereis (char= (char line i) (char line (1- i)))))

(defun prop3 (line)
  (and (not (search "ab" line))
       (not (search "cd" line))
       (not (search "pq" line))
       (not (search "xy" line))))

(defun process (filename)
  (with-open-file (infile filename :direction :input)
    (let ((n 0))
      (loop for line = (read-line infile nil)
            while line
            do (when (and (prop1 line) (prop2 line) (prop3 line))
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
