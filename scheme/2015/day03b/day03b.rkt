#lang racket

(require racket/cmdline
         racket/port
         racket/set
         racket/system)

(define (usage progname)
  (displayln (string-append "usage: " progname " <input file>") (current-error-port))
  (exit 1))

(define (process content)
  (define final-state
    (foldl
      (lambda (ch state)
        (define santa (list-ref state 0))
        (define robo-santa (list-ref state 1))
        (define positions (list-ref state 2))
        (define santa-move (list-ref state 3))
        (if santa-move
          (let* ([new-santa
                  (match ch
                    [#\^ (list (list-ref santa 0) (+ (list-ref santa 1) 1))]
                    [#\v (list (list-ref santa 0) (- (list-ref santa 1) 1))]
                    [#\< (list (- (list-ref santa 0) 1) (list-ref santa 1))]
                    [#\> (list (+ (list-ref santa 0) 1) (list-ref santa 1))]
                    [_ santa])]
                 [new-positions (set-add positions new-santa)])
            (list new-santa robo-santa new-positions #f))
          (let* ([new-robo-santa
                  (match ch
                    [#\^ (list (list-ref robo-santa 0) (+ (list-ref robo-santa 1) 1))]
                    [#\v (list (list-ref robo-santa 0) (- (list-ref robo-santa 1) 1))]
                    [#\< (list (- (list-ref robo-santa 0) 1) (list-ref robo-santa 1))]
                    [#\> (list (+ (list-ref robo-santa 0) 1) (list-ref robo-santa 1))]
                    [_ robo-santa])]
                 [new-positions (set-add positions new-robo-santa)])
            (list santa new-robo-santa new-positions #t))))
    (list '(0 0) '(0 0) (set) #t)
    (string->list content)))
  (set-count (list-ref final-state 2)))

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
