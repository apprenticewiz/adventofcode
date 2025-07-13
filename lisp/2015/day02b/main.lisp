#!/usr/bin/sbcl --script

(defun usage ()
  (format *error-output* "usage: main.lisp <input file>~%")
  (quit :unix-status 1))

(defun split-string (str sep)
  (let ((start 0)
        (parts '()))
    (loop for i from 0 below (length str)
          do (when (char= (char str i) sep)
             (push (subseq str start i) parts)
             (setf start (+ 1 i))))
    (push (subseq str start) parts)
    (nreverse parts)))

(defun parse-dimensions (line)
  (mapcar #'parse-integer (split-string line #\x)))

(defun box-len (line)
  (destructuring-bind (l w h) (parse-dimensions line)
    (let* ((perim1 (* 2 (+ l w)))
           (perim2 (* 2 (+ l h)))
           (perim3 (* 2 (+ w h)))
           (present-len (min perim1 perim2 perim3))
           (bow-len (* l w h)))
      (+ present-len bow-len))))

(defun process (filename)
  (with-open-file (infile filename :direction :input)
    (let ((lines (loop for line = (read-line infile nil)
                       while line
                       collect line)))
      (reduce #'+ lines :key #'box-len))))

(defun main ()
  (let ((args (cdr *posix-argv*)))
    (if (= (length args) 1)
        (let* ((filename (first args))
               (result (process filename)))
          (format t "result = ~A~%" result))
        (usage))))

(main)
