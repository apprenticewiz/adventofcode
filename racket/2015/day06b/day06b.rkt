#lang racket

(require racket/cmdline
         racket/port
         racket/system)

(define (usage progname)
  (displayln (string-append "usage: " progname " <input file>") (current-error-port))
  (exit 1))

(define (perform! grid action r1 c1 r2 c2)
  (for ((row (in-range r1 (add1 r2))))
    (for ((col (in-range c1 (add1 c2))))
      (let* ((idx (+ (* row 1000) col))
             (old-val (vector-ref grid idx)))
        (cond
          ((string=? action "turn on") (vector-set! grid idx (+ old-val 1)))
          ((string=? action "turn off") (vector-set! grid idx (max 0 (- old-val 1))))
          ((string=? action "toggle") (vector-set! grid idx (+ old-val 2))))))))

(define (process content)
  (let ((grid (make-vector (* 1000 1000) 0)))
    (for-each
      (lambda (line)
        (let ((caps (regexp-match #px"(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)" line)))
            (if caps
              (let* ((action (list-ref caps 1))
                     (r1 (string->number (list-ref caps 2)))
                     (c1 (string->number (list-ref caps 3)))
                     (r2 (string->number (list-ref caps 4)))
                     (c2 (string->number (list-ref caps 5))))
                (perform! grid action r1 c1 r2 c2))
              (displayln (string-append "warning: malformed input line: " line) (current-error-port)))))
      (string-split content "\n"))
    (for/sum ((x (in-vector grid))) x)))

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
