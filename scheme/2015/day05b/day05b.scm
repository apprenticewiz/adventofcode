(module day05b
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

(define (prop1 str)
  (define (check-string i str)
    (if (>= i (- (string-length str) 3))
      #f
      (let ((pair1 (substring str i (+ i 2))))
	(define (check-string-inner j str)
	  (if (>= j (- (string-length str) 1))
	    #f
	    (let ((pair2 (substring str j (+ j 2))))
	      (if (string=? pair1 pair2)
		#t
		(check-string-inner (+ 1 j) str)))))
	(define found (check-string-inner (+ i 2) str))
	(if found
	  #t
	  (check-string (+ i 1) str)))))
  (check-string 0 str))

(define (prop2 str)
  (define (check-string i str)
    (if (>= i (- (string-length str) 2))
      #f
      (let ((curr-char (string-ref str i))
            (next-char (string-ref str (+ i 2))))
        (if (char=? curr-char next-char)
          #t
          (check-string (+ i 1) str)))))
  (check-string 0 str))

(define (process filename)
  (let* ((input-file (open-input-file filename))
         (lines (read-lines input-file)))
    (close-input-port input-file)
    (fold-left
      (lambda (count line)
        (if (and (prop1 line) (prop2 line))
          (+ count 1)
          count))
      0
      lines)))
