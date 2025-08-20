(module day05a
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
  (let* ((chars (string->list str))
         (vowel-count
           (fold-left
             (lambda (acc ch)
               (cond
                 ((or (char=? ch #\a) (char=? ch #\e) (char=? ch #\i) (char=? ch #\o) (char=? ch #\u)) (+ acc 1))
                 (else acc)))
             0
             chars)))
    (>= vowel-count 3)))

(define (prop2 str)
  (define (check-string i str)
    (if (>= i (- (string-length str) 1))
      #f
      (let ((curr-char (string-ref str i))
            (next-char (string-ref str (+ i 1))))
        (if (char=? curr-char next-char)
          #t
          (check-string (+ i 1) str)))))
  (check-string 0 str))

(define (prop3 str)
  (and (not (string-contains str "ab"))
       (not (string-contains str "cd"))
       (not (string-contains str "pq"))
       (not (string-contains str "xy"))))

(define (process filename)
  (let* ((input-file (open-input-file filename))
         (lines (read-lines input-file)))
    (close-input-port input-file)
    (fold-left
      (lambda (count line)
        (if (and (prop1 line) (prop2 line) (prop3 line))
          (+ count 1)
          count))
      0
      lines)))
