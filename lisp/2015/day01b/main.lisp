#!/usr/bin/sbcl --script

(defun usage ()
  (format *error-output* "usage: main.lisp <input file>~%")
  (quit :unix-status 1))

(defun process (content)
  (labels ((find-position (chars pos count)
            (cond
              ((minusp count) pos)
              ((null chars) pos)
              (t
                (let* ((ch (car chars))
                       (new-count (cond ((char= ch #\() (+ count 1))
                                        ((char= ch #\)) (- count 1))
                                        (t count))))
                  (find-position (cdr chars) (+ pos 1) new-count))))))
    (find-position (coerce content 'list) 0 0)))

(defun main ()
  (let ((args (cdr *posix-argv*)))
    (if (= (length args) 1)
        (let* ((filename (first args))
               (content (with-open-file (in filename :direction :input)
                          (let ((data (make-string (file-length in))))
                            (read-sequence data in)
                            data)))
               (result (process content)))
          (format t "result = ~A~%" result))
        (usage))))

(main)
