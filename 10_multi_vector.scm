
(define ok #t)

(define (make-multi-vector sizes . xs)
  (define vec
    (if (null? xs)
        (make-vector (+ (apply * sizes) 1) 0) 
        (make-vector (+ (apply * sizes) 1) (car xs))))
  (vector-set! vec 0 'mv)
  vec)


(define (multi-vector-ref m indices)
  (define num (+ (apply * indices) 1))
  (vector-ref m num))

(define (multi-vector? x)
  (and (vector? x) (equal? (vector-ref x 0) 'mv)))

(define (multi-vector-set! m indices x)
  (define num (+ (apply * indices) 1))
  (vector-set! m num x))

(begin
  (write (eval '(begin (define m (make-multi-vector (quote (11 12 9 16)))) (multi-vector? m))
               (interaction-environment)))
  (newline))

(multi-vector? (quote (1 2 3 4)))
(begin
  (define m (make-multi-vector (quote (11 11 11 11 11)) (quote x)))
  (do ((i 0 (+ i 1)))
    ((>= i 11) ok)
    (set! ok (and ok (equal? (multi-vector-ref m (list i i i i i)) (quote x))))))
;(begin (define m (make-multi-vector (quote (11 12 9 16)))) (multi-vector? m))

;(define m (make-multi-vector '(2 2 2) 1))
;(make-multi-vector '(2 2 2) 3)
;(vector-ref #(1 2 3) 2)
;(multi-vector-set! m '(0 0 0) 9)
;(multi-vector-ref m '(0 0 0))

;guile 1.8 -l или -s