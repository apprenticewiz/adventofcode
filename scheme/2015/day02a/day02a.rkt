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
      (let* ([dims (map string->number (string-split line "x"))]
             [l (list-ref dims 0)]
             [w (list-ref dims 1)]
             [h (list-ref dims 2)]
             [area1 (* l w)]
             [area2 (* l h)]
             [area3 (* w h)]
             [surface-area (+ (* 2 area1) (* 2 area2) (* 2 area3))]
             [min-area (apply min (list area1 area2 area3))])
        (+ acc surface-area min-area)))
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
