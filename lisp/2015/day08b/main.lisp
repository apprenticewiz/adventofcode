#!/usr/bin/sbcl --script

(defun usage ()
  (format *error-output* "usage: main.lisp <input file>~%")
  (quit :unix-status 1))

(defun scan-line (line)
  (let ((code-len (length line))
        (enc-len 0))
    (loop for ch in line
          do (cond ((or (char= ch #\\) (char= ch #\"))
                    (setf enc-len (+ enc-len 2)))
                   (t (setf enc-len (+ enc-len 1)))))
    (- enc-len code-len)))

(defun process (filename)
  (with-open-file (infile filename :direction :input)
    (let ((res 0))
      (loop for line = (read-line infile nil)
            while line
            do (setf res (+ res 2 (scan-line (coerce line 'list)))))
      res)))

(defun main ()
  (let ((args (cdr *posix-argv*)))
    (if (= (length args) 1)
        (let* ((filename (first args))
               (result (process filename)))
          (format t "result = ~A~%" result))
        (usage))))

(main)
