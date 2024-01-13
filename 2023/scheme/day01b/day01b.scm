(module day01b (main main))

(define (prog-usage progname)
    (display (format "usage: ~a <file>" progname))
    (newline)
    (exit))

(define (string-find needle haystack)
    (if (string-contains haystack needle)
        (do ((i 0 (+ i 1)))
            ((substring-at? haystack needle i) i))
        'nil))

(define (string-rfind needle haystack)
    (if (string-contains haystack needle)
        (do ((i (- (string-length haystack) 1) (- i 1)))
            ((substring-at? haystack needle i) i))
        'nil))

(define (word-to-digit str)
    (cond ((string=? str "zero") "0")
          ((string=? str "one") "1")
          ((string=? str "two") "2")
          ((string=? str "three") "3")
          ((string=? str "four") "4")
          ((string=? str "five") "5")
          ((string=? str "six") "6")
          ((string=? str "seven") "7")
          ((string=? str "eight") "8")
          ((string=? str "nine") "9")))

(define (process filename)
    (let ((result 0)
          (digits '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
                    "zero" "one" "two" "three" "four" "five"
                    "six" "seven" "eight" "nine")))
        (with-input-from-file filename
            (lambda ()
                (let loop ((line (read-line (current-input-port))))
                    (if (not (eof-object? line))
                        (begin
                            (let ((min-index 'nil)
                                  (max-index 'nil)
                                  (left-digit 'nil)
                                  (right-digit 'nil))
                                (for-each
                                    (lambda (digit)
                                        (let ((left-index (string-find digit line))
                                              (right-index (string-rfind digit line)))
                                            (if (and (not (eq? left-index 'nil))
                                                     (or (eq? min-index 'nil)
                                                         (< left-index min-index)))
                                                (begin
                                                    (set! min-index left-index)
                                                    (set! left-digit
                                                        (if (> (string-length digit) 1)
                                                            (word-to-digit digit)
                                                            digit))))
                                            (if (and (not (eq? right-index 'nil))
                                                     (or (eq? max-index 'nil)
                                                         (> right-index max-index)))
                                                (begin
                                                    (set! max-index right-index)
                                                    (set! right-digit
                                                        (if (> (string-length digit) 1)
                                                            (word-to-digit digit)
                                                            digit))))))
                                    digits)
                                (let ((numstr (format "~a~a" left-digit right-digit)))
                                    (set! result (+ result (string->number numstr))))
                                (loop (read-line (current-input-port))))
                        (close-input-port (current-input-port)))))))
        result))

(define (main argv)
    (if (< (length argv) 2)
        (prog-usage (car argv))
        (let* ((filename (cadr argv))
               (result (process filename)))
            (display (format "result = ~a" result))
            (newline))))
