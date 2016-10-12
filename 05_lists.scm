(define (my-range a b d)
  (if (>= a b)
      '()
      (cons a (my-range (+ a d) b d))))

(define (my-flatten x)
  (cond ((null? x) '())
        ((list? (car x)) `(,@(my-flatten (car x)) ,@(my-flatten (cdr x))))
        (else (cons (car x) (my-flatten (cdr x))))))

(define (my-element? x xs)
  (cond ((null? xs) #f)
        ((equal? (car xs) x) #t)
        (else (my-element? x (cdr xs)))))

(define (my-filter pred? xs)
  (cond ((null? xs) '())
        ((pred? (car xs)) (cons (car xs) (my-filter pred? (cdr xs))))
        (else (my-filter pred? (cdr xs)))))

(define (my-fold-right op xs)
  (cond ((null? (cdr xs)) (car xs)) 
        (else (op (car xs) (my-fold-right op (cdr xs))))))

(define (my-fold-left op xs)
  (define (rec r op xs)
    (if (null? xs)
        r
        (rec (op r  (car xs)) op (cdr xs))))
  (rec (car xs) op (cdr xs)))

(my-range 5 10 5)
(my-flatten '((1) 2 (3 (4 5)) 6))
(my-element? 1 '(3 2 1))
(my-element? 4 '(3 2 1))

(my-filter odd? (my-range 0 10 1))
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))

(my-fold-left  quotient '(16 2 2 2 2))
(my-fold-right expt     '(2 3 4)) 
