(module day04a
  (main main))

(define (main args)
  (let* ((argc (length args))
         (progname (car args)))
    (if (< argc 2)
      (usage progname))
      (let* ((key (cadr args))
             (result (process key)))
        (printf "result = ~a\n" (number->string result)))))

(define (usage progname)
  (fprintf (current-error-port) "usage: ~a <key>\n" progname)
  (exit 1))

(define (process key)
  (define (check-key k n)
    (let* ((try-key (string-append k (number->string n)))
           (digest (md5sum-string try-key)))
      (if (string-prefix? "00000" digest)
        n
        (check-key k (+ n 1)))))
  (check-key key 0))
