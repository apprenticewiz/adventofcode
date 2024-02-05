#!/usr/bin/env clisp

(setf *progname* "day04b.lisp")

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

(defun seq (from-val to-val)
  (loop for i from from-val to to-val
    collect i))

(defun update-alist (key new-value alist)
  (acons key new-value (remove key alist :key #'car)))

(defun process (filename)
  (let ((instances 
          (foldl
            (lambda (curr-instances line)
              (let* ((card-part (car (split-string #\: line)))
                     (card-num-str (cadr (filter (lambda (x) (not (= (length x) 0))) (split-string #\Space card-part))))
                     (card-num (parse-integer card-num-str))
                     (rest (cadr (split-string #\: line)))
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
                (foldl
                  (lambda (prev-instances i)
                    (let ((copies 
                            (+
                              (if (null (assoc i prev-instances))
                                0
                                (cdr (assoc i prev-instances)))
                              1
                              (if (null (assoc card-num prev-instances))
                                0
                                (cdr (assoc card-num prev-instances))))))
                      (update-alist i copies prev-instances)))
                  curr-instances
                  (seq (+ card-num 1) (+ card-num common-count)))))
            nil
            (read-file-into-lines filename))))
    (+
      (foldl
        (lambda (result pair)
          (+ result (cdr pair)))
        0
        instances)
      (length (read-file-into-lines filename)))))

(defun main ()
  (if (< (length *args*) 1)
    (prog-usage))
  (setf filename (car *args*))
  (setf result (process filename))
  (format t "result = ~d" result))

(main)
