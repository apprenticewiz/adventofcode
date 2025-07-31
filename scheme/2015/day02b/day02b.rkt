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
             [perim1 (* 2 (+ l w))]
             [perim2 (* 2 (+ l h))]
             [perim3 (* 2 (+ w h))]
             [present-len (apply min (list perim1 perim2 perim3))]
             [bow-len (* l w h)])
        (+ acc present-len bow-len)))
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
