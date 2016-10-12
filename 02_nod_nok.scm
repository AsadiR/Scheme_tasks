(define (my-gcd a b)
  (define (rec-my-gcd a b)
    (define res (remainder a b))
    (if (= 0 res)
        b
        (rec-my-gcd b res)))
  (if (> a b)
      (rec-my-gcd a b)
      (rec-my-gcd b a)))

(define (my-lcm a b)
  (/ (abs (* a b)) (my-gcd a b)))

(define (prime? a)
  (define sqrt_of_a (sqrt a))
  (define (rec-prime? a i)
    (cond
      ((> i sqrt_of_a) #t)
      ((not (= 0 (remainder a i))) (rec-prime? a (+ i 1)))
      (else #f)))
  (rec-prime? a 2))

(my-gcd 3542 2464)
(my-lcm 3 4)
(prime? 11) 
(prime? 4)       