(module day01b
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
    (define (calc-pos current-floor current-pos xs)
      (cond
        ((null? xs) 0)
        ((< current-floor 0) current-pos)
        (else
          (let* ((ch (car xs))
                 (rest (cdr xs))
                 (next-pos (+ 1 current-pos))
                 (next-floor
                   (cond
                     ((char=? ch #\() (+ current-floor 1))
                     ((char=? ch #\)) (- current-floor 1))
                     (else current-floor))))
            (calc-pos next-floor next-pos rest)))))
    (close-input-port input-file)
    (calc-pos 0 0 (string->list contents))))
