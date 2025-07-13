#!/usr/bin/sbcl --script

(defun usage ()
  (format *error-output* "usage: main.lisp <input file>~%")
  (quit :unix-status 1))

(defun process (content)
  (labels ((proc-input (chars count)
            (if (null chars)
              count
              (let* ((ch (car chars))
                     (new-count
                       (cond
                         ((char= ch #\() (+ count 1))
                         ((char= ch #\)) (- count 1))
                         (t count))))
                (proc-input (cdr chars) new-count)))))
    (proc-input (coerce content 'list) 0)))

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
