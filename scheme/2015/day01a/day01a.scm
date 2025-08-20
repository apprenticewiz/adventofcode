(module day01a
  (main main))

(define (main args)
  (let* ((argc (length args))
         (progname (car args)))
    (if (< argc 2)
      (usage progname))
      (let* ((filename (cadr args))
             (result (process filename)))
        (printf "result = ~a\n" (number->string result)))))

(define (usage progname)
  (fprintf (current-error-port) "usage: ~a <input file>\n" progname)
  (exit 1))

(define (fold-left op u l)
  (if (null? l)
    u
    (let ((x (car l))
	  (xs (cdr l)))
      (fold-left op (op u x) xs))))

(define (process filename)
  (let* ((input-file (open-input-file filename))
         (contents (read-string input-file)))
    (close-input-port input-file)
    (fold-left
      (lambda (current-floor ch)
	(cond
	  ((char=? ch #\() (+ current-floor 1))
	  ((char=? ch #\)) (- current-floor 1))
	  (else current-floor)))
      0
      (string->list contents))))
