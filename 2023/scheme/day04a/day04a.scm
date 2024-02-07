(module day04a (main main))

(define (prog-usage progname)
    (display (format "usage: ~a <file>" progname))
    (newline)
    (exit))

(define (read-file-into-lines filename)
  (call-with-input-file filename
    (lambda (file)
      (let loop ((lines '())
                 (line (read-line file)))
        (if (not (eof-object? line))
            (begin
                (loop (cons line lines) (read-line file)))
            (reverse lines))))))

(define (foldl f z xs)
  (if (null? xs)
    z
    (foldl f (f z (car xs)) (cdr xs))))

(define (mapcar f xs)
    (if (null? xs)
        xs
        (cons (f (car xs)) (mapcar f (cdr xs)))))

(define (process filename)
    (foldl
        (lambda (result line)
            (let* ((rest (cadr (string-split line ":")))
                   (winning-str (car (string-split rest "|")))
                   (hand-str (cadr (string-split rest "|")))
                   (winning-nums (mapcar string->number (string-split winning-str " ")))
                   (hand-nums (mapcar string->number (string-split hand-str " ")))
                   (common-count
                        (foldl
                            (lambda (result num)
                                (if (member num hand-nums)
                                    (+ 1 result)
                                    result))
                            0
                            winning-nums)))
                (if (> common-count 0)
                    (+ result (expt 2 (- common-count 1)))
                    result)))
        0
        (read-file-into-lines filename)))

(define (main argv)
    (if (< (length argv) 2)
        (prog-usage (car argv))
        (let* ((filename (cadr argv))
               (result (process filename)))
            (display (format "result = ~a" result))
            (newline))))
