#lang racket

(require racket/cmdline
   racket/port
   racket/system)

(define (usage progname)
  (displayln (string-append "usage: " progname " <input file>") (current-error-port))
  (exit 1))

(define (check-pos chars pos floors)
  (cond
    [(null? chars) 0]
    [(< floors 0) pos]
    [else
      (let* ([x (car chars)]
             [xs (cdr chars)]
             [new-floors
               (match x [#\( (+ floors 1)]
                        [#\) (- floors 1)]
                        [_ floors])])
        (check-pos xs (+ 1 pos) new-floors))]))

(define (process content)
  (check-pos (string->list content) 0 0))

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
