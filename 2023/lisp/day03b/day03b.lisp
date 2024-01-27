#!/usr/bin/env clisp

(setf *progname* "day03b.lisp")

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

(defun build-numbers (lines)
  (setf number-locs nil)
  (setf row 0)
  (setf scanning-number nil)
  (setf number nil)
  (setf current-pos '(-1 -1))
  (dolist (line lines)
    (loop for col below (length line)
      do
        (let ((ch (char line col)))
          (if scanning-number
            (if (not (null (digit-char-p ch)))
              (setf number (concatenate 'string number (string ch)))
              (progn
                (setf number-locs (acons current-pos number number-locs))
                (setf scanning-number nil)
                (setf number nil)))
            (if (not (null (digit-char-p ch)))
              (progn
                (setf number (concatenate 'string number (string ch)))
                (setf current-pos `(,row ,col))
                (setf scanning-number t))))))
    (if scanning-number
      (progn
        (setf number-locs (acons current-pos number number-locs))
        (setf scanning-number nil)
        (setf number nil)))
    (setf row (+ 1 row)))
  number-locs)

(defun build-parts (lines)
  (setf part-locs nil)
  (setf row 0)
  (dolist (line lines)
    (loop for col below (length line)
      do
        (let ((ch (char line col)))
          (if (char= #\* ch)
            (let ((current-pos `(,row ,col)))
              (setf part-locs (acons current-pos ch part-locs))))))
    (setf row (+ 1 row)))
  part-locs)

(defun check-parts (number-locs part-locs)
  (setf result 0)
  (setf neighbors '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))
  (dolist (part-loc-entry part-locs)
    (let* ((part-loc (car part-loc-entry))
           (part-row (car part-loc))
           (part-col (cadr part-loc)))
      (setf adjacent-count 0)
      (setf prod 1)
      (dolist (number-loc-entry number-locs)
        (let* ((number-loc (car number-loc-entry))
               (number (cdr number-loc-entry))
               (number-row (car number-loc))
               (number-col-first (cadr number-loc))
               (number-col-last (+ (cadr number-loc) (length number) -1)))
          (setf found nil)
          (dolist (neighbor neighbors)
            (let ((adjacent-row (+ part-row (car neighbor)))
                  (adjacent-col (+ part-col (cadr neighbor))))
              (loop for number-col from number-col-first to number-col-last
                do
                  (if (and (= adjacent-row number-row) (= adjacent-col number-col))
                    (progn
                      (setf adjacent-count (+ 1 adjacent-count))
                      (setf prod (* prod (parse-integer number)))
                      (setf found t)
                      (return)))))
            (if found
              (return)))))
      (if (= adjacent-count 2)
        (setf result (+ result prod)))))
  result)

(defun process (filename)
  (setf lines (read-file-into-lines filename))
  (setf number-locs (build-numbers lines))
  (setf part-locs (build-parts lines))
  (check-parts number-locs part-locs))

(defun main ()
  (if (< (length *args*) 1)
    (prog-usage))
  (setf filename (car *args*))
  (setf result (process filename))
  (format t "result = ~d" result))

(main)
