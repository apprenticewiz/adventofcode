#lang racket

(require file/sha1 
         openssl/md5
         racket/cmdline
         racket/port
         racket/string
         racket/system)

(define (usage progname)
  (displayln (string-append "usage: " progname " <key>") (current-error-port))
  (exit 1))

(define (check-key key n)
  (define try-key (string-append key (number->string n)))
  (define digest (md5-bytes (open-input-bytes (string->bytes/utf-8 try-key))))
  (define hex-digest (bytes->hex-string digest))
  (if (string-prefix? hex-digest "00000")
    n
    (check-key key (+ n 1))))

(define (process key)
  (check-key key 1))

(define (main)
  (define progname (find-system-path 'run-file))
  (define args (current-command-line-arguments))
  (cond
    [(= (vector-length args) 1)
      (define key (vector-ref args 0))
      (define result (process key))
      (printf "result = ~a\n" result)]
    [else (usage (path->string progname))]))

(main)
