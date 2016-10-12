
(define fname "17input.txt")
(define fp (open-input-file fname))

(define (read-words)
  (define word (read))
  (cond ((eof-object? word) '())
        (else (cons word (read-words)))))

(read-words)