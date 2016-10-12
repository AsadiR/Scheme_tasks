(define (to-vector seq)
  (cond ((vector? seq) seq)
        ((list? seq) (list->vector seq))
        ((string? seq) (list->vector (string->list seq)))
        (else 'error)))

(define (make-source seq . xs)
  (if (null? xs)
      (cons 0 (cons #f (list (to-vector seq))))
      (cons 0 (cons (car xs) (list (to-vector seq))))))

(define (peek src)
  (if (< (car src) (vector-length (caddr src)))
      (vector-ref (caddr src) (car src))
      (cadr src)))

(define (next src)
  (define index (car src))
  (set-car! src (+ index 1))
  (if (< index (vector-length (caddr src)))
      (vector-ref (caddr src) index)
      (cadr src)))


(define s (make-source "abc" 'end))
s
(peek s)
(next s)
(peek s)
(next s)
(peek s)
(next s)
(peek s)
(next s)
