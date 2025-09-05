#!/usr/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

(ql:quickload "cl-ppcre")
(ql:quickload "uiop")

(defparameter *operations* (make-hash-table :test 'equal))
(defparameter *cache* (make-hash-table :test 'equal))

(defparameter *patterns*
  (list
    (cons "^(\\d+|\\w+) -> (\\w+)$"
          (lambda (matches)
            (let ((src (aref matches 0))
                  (dest (aref matches 1)))
               (setf (gethash dest *operations*)
                     (list "ASSIGN" src)))))
    (cons "^NOT (\\d+|\\w+) -> (\\w+)$"
          (lambda (matches)
            (let ((src (aref matches 0))
                  (dest (aref matches 1)))
               (setf (gethash dest *operations*)
                     (list "NOT" src)))))
    (cons "^(\\d+|\\w+) (AND|OR) (\\d+|\\w+) -> (\\w+)$"
          (lambda (matches)
            (let ((src1 (aref matches 0))
                  (op (aref matches 1))
                  (src2 (aref matches 2))
                  (dest (aref matches 3)))
              (setf (gethash dest *operations*)
                    (list op src1 src2)))))
    (cons "^(\\d+|\\w+) (LSHIFT|RSHIFT) (\\d+) -> (\\w+)$"
          (lambda (matches)
            (let ((src (aref matches 0))
                  (op (aref matches 1))
                  (amt (first (multiple-value-list (parse-integer (aref matches 2)))))
                  (dest (aref matches 3)))
              (setf (gethash dest *operations*)
                    (list op src amt)))))))

(defun parse-line (line)
  (loop for (re . handler) in *patterns*
	for match = (second (multiple-value-list (cl-ppcre:scan-to-strings re line)))
	when match
	  do (funcall handler match)
	     (return t)
	finally (progn
		  (format *error-output* "error: malformed line: ~a~%" line)
		  (uiop:quit 1))))

(defun evaluate (expr)
  (or (first (multiple-value-list (ignore-errors (parse-integer expr))))
      (first (multiple-value-list (gethash expr *cache*)))
      (let* ((op (first (multiple-value-list (gethash expr *operations*))))
	     (operator (first op))
	     (result
	       (cond
		 ((string= operator "ASSIGN")
		  (evaluate (second op)))
		 ((string= operator "NOT")
		   (lognot (evaluate (second op))))
		 ((string= operator "AND")
		   (logand (evaluate (second op))
			   (evaluate (third op))))
		 ((string= operator "OR")
		   (logior (evaluate (second op))
			   (evaluate (third op))))
		 ((string= operator "LSHIFT")
		   (ash (evaluate (second op)) (third op)))
		 ((string= operator "RSHIFT")
		   (ash (evaluate (second op)) (- (third op))))))
	     (masked (logand result #xffff)))
	(setf (gethash expr *cache*) masked)
	masked)))

(defun process (filename)
  (clrhash *operations*)
  (clrhash *cache*)
  (with-open-file (infile filename)
    (loop for line = (read-line infile nil)
          while line do
            (parse-line line)))
  (evaluate "a"))

(defun usage ()
  (format *error-output* "usage: main.lisp <input file>~%")
  (uiop:quit 1))

(defun main ()
  (let ((args (cdr *posix-argv*)))
    (if (= (length args) 1)
        (let* ((filename (first args))
               (result (process filename)))
          (format t "result = ~A~%" result))
        (usage))))

(main)
