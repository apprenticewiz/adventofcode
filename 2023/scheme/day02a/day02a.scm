(module day02a (main main))

(define *total-red* 12)
(define *total-green* 13)
(define *total-blue* 14)

(define (prog-usage progname)
    (display (format "usage: ~a <file>" progname))
    (newline)
    (exit))

(define (process filename)
    (let ((result 0))
        (with-input-from-file filename
            (lambda ()
                (let loop ((line (read-line (current-input-port))))
                    (if (not (eof-object? line))
                        (begin
                            (let* ((game-part (car (string-split line ":")))
                                   (draws-part (cadr (string-split line ":")))
                                   (game-num-part (cadr (string-split game-part " ")))
                                   (game-num (string->number game-num-part))
                                   (valid #t)
                                   (draws (string-split draws-part ";")))
                                (for-each
                                    (lambda (draw)
                                        (let ((color-amounts (string-split draw ",")))
                                            (for-each
                                                (lambda (color-amount)
                                                    (let* ((trimmed-color-amount (substring color-amount 1))
                                                           (amount-str (car (string-split trimmed-color-amount " ")))
                                                           (color (cadr (string-split trimmed-color-amount " ")))
                                                           (amount (string->number amount-str)))
                                                        (cond ((string=? color "red")
                                                                (if (> amount *total-red*)
                                                                    (set! valid #f)))
                                                              ((string=? color "green")
                                                                (if (> amount *total-green*)
                                                                    (set! valid #f)))
                                                              ((string=? color "blue")
                                                                (if (> amount *total-blue*)
                                                                    (set! valid #f))))))
                                                color-amounts)))
                                    draws)
                                (if valid
                                    (set! result (+ result game-num))))
                            (loop (read-line (current-input-port))))
                        (close-input-port (current-input-port))))))
        result))

(define (main argv)
    (if (< (length argv) 2)
        (prog-usage (car argv))
        (let* ((filename (cadr argv))
               (result (process filename)))
            (display (format "result = ~a" result))
            (newline))))
