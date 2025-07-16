#!/usr/bin/sbcl --script

(require :asdf)
(asdf:oos 'asdf:load-op 'ironclad)

(defun usage ()
  (format *error-output* "usage: main.lisp <key>~%")
  (quit :unix-status 1))

(defun string-prefix-p (prefix string)
  (and (stringp prefix)
       (stringp string)
       (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun process (key)
  (let ((n 1))
    (loop
      (let* ((try-key (with-output-to-string (s)
                        (format s "~a~d" key n)))
             (digest (ironclad:byte-array-to-hex-string
                       (ironclad:digest-sequence
                         :md5
                         (ironclad:ascii-string-to-byte-array try-key)))))
        (when (string-prefix-p "000000" digest)
          (return n)))
      (incf n))))

(defun main ()
  (let ((args (cdr *posix-argv*)))
    (if (= (length args) 1)
        (let* ((key (first args))
               (result (process key)))
          (format t "result = ~A~%" result))
        (usage))))

(main)
