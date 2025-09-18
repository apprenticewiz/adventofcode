#!/usr/bin/sbcl --script

(defun usage ()
  (format *error-output* "usage: main.lisp <input file>~%")
  (quit :unix-status 1))

(defun scan-line (line)
  (let ((code-len (length line))
        (mem-len (labels ((count-escaped (i chars)
                            (cond ((null chars) i)
                              ((= (length chars) 1) (+ i 1))
                              (t
                                (let ((ch1 (car chars))
                                      (ch2 (cadr chars)))
                                  (if (char= ch1 #\\)
                                    (cond ((or (char= ch2 #\\)
                                               (char= ch2 #\"))
                                            (count-escaped (+ i 1) (cddr chars)))
                                          ((char= ch2 #\x)
                                            (count-escaped (+ i 1) (cddddr chars)))
                                          (t
                                            (count-escaped (+ i 1) (cdr chars))))
                                    (count-escaped (+ i 1) (cdr chars))))))))
                    (count-escaped 0 (butlast (cdr line))))))
    (- code-len mem-len)))

(defun process (filename)
  (with-open-file (infile filename :direction :input)
    (let ((res 0))
      (loop for line = (read-line infile nil)
            while line
            do (setf res (+ res (scan-line (coerce line 'list)))))
      res)))

(defun main ()
  (let ((args (cdr *posix-argv*)))
    (if (= (length args) 1)
        (let* ((filename (first args))
               (result (process filename)))
          (format t "result = ~A~%" result))
        (usage))))

(main)
