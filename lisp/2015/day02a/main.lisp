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

(defun box-area (line)
  (destructuring-bind (l w h) (parse-dimensions line)
    (let* ((area1 (* l w))
           (area2 (* l h))
           (area3 (* w h))
           (surface-area (+ (* 2 area1) (* 2 area2) (* 2 area3)))
           (min-area (min area1 area2 area3)))
      (+ surface-area min-area))))

(defun process (filename)
  (with-open-file (infile filename :direction :input)
    (let ((lines (loop for line = (read-line infile nil)
                       while line
                       collect line)))
      (reduce #'+ lines :key #'box-area))))

(defun main ()
  (let ((args (cdr *posix-argv*)))
    (if (= (length args) 1)
        (let* ((filename (first args))
               (result (process filename)))
          (format t "result = ~A~%" result))
        (usage))))

(main)
