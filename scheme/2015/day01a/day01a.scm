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

(define (process filename)
  (let* ((input-file (open-input-file filename))
         (contents (read-string input-file)))
    (define (calc-floor current-floor xs)
      (if (null? xs)
        current-floor
        (let* ((ch (car xs))
               (rest (cdr xs))
               (next-floor
                 (cond
                   ((char=? ch #\() (+ current-floor 1))
                   ((char=? ch #\)) (- current-floor 1))
                   (else current-floor))))
          (calc-floor next-floor rest))))
    (close-input-port input-file)
    (calc-floor 0 (string->list contents))))
