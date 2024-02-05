#!/usr/bin/env clisp

(setf *progname* "day04a.lisp")

(defun prog-usage ()
  (format t "usage: ~a <file>" *progname*)
  (exit))

(defun read-file-into-lines (filename)
  (with-open-file (file filename :direction :input)
    (setf lines
      (loop for line = (read-line file nil)
          while line
          collect line))
    (close file))
  lines)

(defun split-string (delimiter str)
  (loop with start = 0
        with end = (position delimiter str)
        with result = '()
        while end do
          (push (subseq str start end) result)
          (setf start (1+ end)
                end (position delimiter str :start start))
        finally (push (subseq str start) result)
        finally (return (nreverse result))))

(defun foldl (f z xs)
  (if (null xs)
    z
    (foldl f (funcall f z (car xs)) (cdr xs))))

(defun filter (f args)                                                      
  (cond ((null args) nil)
    ((if (funcall f (car args))
      (cons (car args) (filter  f (cdr args)))
      (filter f (cdr args))))))

(defun process (filename)
  (foldl
    (lambda (result line)
      (let* ((rest (cadr (split-string #\: line)))
             (winning-str (car (split-string #\| rest)))
             (hand-str (cadr (split-string #\| rest)))
             (winning-nums
                (mapcar #'parse-integer
                  (filter (lambda (x) (not (= (length x) 0))) (split-string #\Space winning-str))))
             (hand-nums
                (mapcar #'parse-integer
                  (filter (lambda (x) (not (= (length x) 0))) (split-string #\Space hand-str))))
             (common-count
                (foldl
                  (lambda (n num)
                    (if (not (null (position num hand-nums)))
                      (+ n 1)
                      n))
                  0
                  winning-nums)))
        (if (> common-count 0)
          (+ result (ash 1 (- common-count 1)))
          result)))
    0
    (read-file-into-lines filename)))

(defun main ()
  (if (< (length *args*) 1)
    (prog-usage))
  (setf filename (car *args*))
  (setf result (process filename))
  (format t "result = ~d" result))

(main)
