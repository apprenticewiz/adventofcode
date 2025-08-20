(module day02a
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
         (lines (read-lines input-file)))
    (close-input-port input-file)
    (fold-left
      (lambda (total-area line)
        (let* ((dims (string-split line "x"))
               (l (string->number (car dims)))
               (w (string->number (cadr dims)))
               (h (string->number (caddr dims)))
               (area1 (* l w))
               (area2 (* l h))
               (area3 (* w h))
               (surface-area (+ (* 2 area1) (* 2 area2) (* 2 area3)))
               (min-area (min area1 area2 area3)))
          (+ total-area surface-area min-area)))
      0
      lines)))
