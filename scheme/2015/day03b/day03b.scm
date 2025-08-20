(module day03b
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

(define (fold-left op u l)
  (if (null? l)
    u
    (let ((x (car l))
          (xs (cdr l)))
      (fold-left op (op u x) xs))))

(define (process filename)
  (let* ((input-file (open-input-file filename))
         (contents (read-string input-file)))
    (close-input-port input-file)
    (define positions (make-hashtable))
    (define (make-key pair)
      (string-append "(" (number->string (car pair)) "," (number->string (cdr pair)) ")"))
    (hashtable-put! positions (make-key '(0 . 0)) #t)
    (fold-left
      (lambda (state ch)
        (let* ((santa (car state))
               (robo-santa (cadr state))
               (santa-move (caddr state)))
          (cond
            (santa-move
              (let ((next-pos
                      (cond
                        ((char=? ch #\^) (cons (car santa) (+ (cdr santa) 1)))
                        ((char=? ch #\v) (cons (car santa) (- (cdr santa) 1)))
                        ((char=? ch #\<) (cons (+ (car santa) 1) (cdr santa)))
                        ((char=? ch #\>) (cons (- (car santa) 1) (cdr santa))))))
                (hashtable-put! positions (make-key next-pos) #t)
                (list next-pos robo-santa #f)))
            (else
              (let ((next-pos
                      (cond
                        ((char=? ch #\^) (cons (car robo-santa) (+ (cdr robo-santa) 1)))
                        ((char=? ch #\v) (cons (car robo-santa) (- (cdr robo-santa) 1)))
                        ((char=? ch #\<) (cons (+ (car robo-santa) 1) (cdr robo-santa)))
                        ((char=? ch #\>) (cons (- (car robo-santa) 1) (cdr robo-santa))))))
                (hashtable-put! positions (make-key next-pos) #t)
                (list santa next-pos #t))))))
      (list '(0 . 0) '(0 . 0) #t)
      (string->list contents))
    (hashtable-size positions)))
