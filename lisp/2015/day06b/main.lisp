#!/usr/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

(ql:quickload "cl-ppcre")

(defun usage ()
  (format *error-output* "usage: main.lisp <input file>~%")
  (quit :unix-status 1))

(defun perform (grid action r1 c1 r2 c2)
  (dotimes (i (+ (- r2 r1) 1))
    (dotimes (j (+ (- c2 c1) 1))
      (let* ((row (+ r1 i))
             (col (+ c1 j))
             (old-value (aref grid row col)))
        (cond ((string= action "turn on") (setf (aref grid row col) (+ old-value 1)))
              ((string= action "turn off") (setf (aref grid row col) (if (> old-value 0) (- old-value 1) 0)))
              ((string= action "toggle") (setf (aref grid row col) (+ old-value 2))))))))

(defun sum-values (grid)
  (let ((total 0))
    (dotimes (row 1000)
      (dotimes (col 1000)
        (let ((n (aref grid row col)))
          (setq total (+ n total)))))
    total))

(defun process (filename)
  (with-open-file (infile filename :direction :input)
    (let ((grid (make-array '(1000 1000) :initial-element 0)))
      (loop for line = (read-line infile nil)
            while line
            do
              (let*
                ((result (multiple-value-list
                           (cl-ppcre:scan-to-strings
                             "(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)"
                              line)))
                 (tokens (second result))
                 (action (aref tokens 0))
                 (r1 (first (multiple-value-list (parse-integer (aref tokens 1)))))
                 (c1 (first (multiple-value-list (parse-integer (aref tokens 2)))))
                 (r2 (first (multiple-value-list (parse-integer (aref tokens 3)))))
                 (c2 (first (multiple-value-list (parse-integer (aref tokens 4))))))
                (perform grid action r1 c1 r2 c2)))
      (sum-values grid))))

(defun main ()
  (let ((args (cdr *posix-argv*)))
    (if (= (length args) 1)
        (let* ((filename (first args))
               (result (process filename)))
          (format t "result = ~A~%" result))
        (usage))))

(main)
