#lang racket

(require racket/cmdline
         racket/port
         racket/system)

(define (usage progname)
  (displayln (string-append "usage: " progname " <input file>") (current-error-port))
  (exit 1))

(define (process content)
  (foldl
    (lambda (line acc)
      (let* ((code-len (string-length line))
             (enc-str (format "~s" line))
             (enc-len (string-length enc-str)))
        (+ acc (- enc-len code-len))))
    0
    (string-split content)))

(define (main)
  (define progname (find-system-path 'run-file))
  (define args (current-command-line-arguments))
  (cond
    [(= (vector-length args) 1)
      (define filename (vector-ref args 0))
      (define content (file->string filename))
      (define result (process content))
      (printf "result = ~a\n" result)]
    [else (usage (path->string progname))]))

(main)
