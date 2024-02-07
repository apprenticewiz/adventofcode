(module day04b (main main))

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

(define (count-lines-in-file file-path)
  (with-input-from-file file-path
    (lambda ()
      (let loop ((line (read-line (current-input-port)))
                 (count 0))
        (if (eof-object? line)
            count
            (loop (read-line (current-input-port))
                  (+ count 1)))))))

(define (foldl f z xs)
  (if (null? xs)
    z
    (foldl f (f z (car xs)) (cdr xs))))

(define (mapcar f xs)
    (if (null? xs)
        xs
        (cons (f (car xs)) (mapcar f (cdr xs)))))

(define (range start end)
  (if (> start end)
      '()
      (cons start (range (+ start 1) end))))

(define (update-alist key new-value xs)
    (mapcar
        (lambda (entry)
            (if (equal? (car entry) key)
                `(,key ,new-value)
                entry))
        xs))

(define (process filename)
    (let ((instances
            (foldl
               (lambda (prev-instances line)
                    (let* ((card-part (car (string-split line ":")))
                           (card-num (string->number (cadr (string-split card-part " "))))
                           (rest (cadr (string-split line ":")))
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
                        (foldl
                            (lambda (prev-inst i)
                                (let
                                    ((copies
                                        (+ (if (assoc i prev-inst)
                                               (cadr (assoc i prev-inst))
                                               0)
                                           1
                                           (if (assoc card-num prev-inst)
                                               (cadr (assoc card-num prev-inst))
                                               0))))
                                    (if (assoc i prev-inst)
                                        (update-alist i copies prev-inst)
                                        (cons `(,i ,copies) prev-inst))))
                            prev-instances
                            (range (+ card-num 1) (+ card-num common-count)))))
               '()
               (read-file-into-lines filename))))
        (+
            (foldl
                (lambda (result entry)
                    (+ result (cadr entry)))
                0
                instances)
            (count-lines-in-file filename))))

(define (main argv)
    (if (< (length argv) 2)
        (prog-usage (car argv))
        (let* ((filename (cadr argv))
               (result (process filename)))
            (display (format "result = ~a" result))
            (newline))))
