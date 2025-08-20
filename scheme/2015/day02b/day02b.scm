(module day02b
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
      (lambda (total-len line)
        (let* ((dims (string-split line "x"))
               (l (string->number (car dims)))
               (w (string->number (cadr dims)))
               (h (string->number (caddr dims)))
               (perim1 (* 2 (+ l w)))
               (perim2 (* 2 (+ l h)))
               (perim3 (* 2 (+ w h)))
               (present-len (min perim1 perim2 perim3))
               (bow-len (* l w h)))
          (+ total-len present-len bow-len)))
      0
      lines)))
