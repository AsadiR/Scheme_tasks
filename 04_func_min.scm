(define (newton_min f f′ f′′ x ε)
  (cond
    ((< (abs (f′ x)) ε) x)
    (else (newton_min f
                  f′
                  f′′
                  (- x (/ (f′ x) (f′′ x)))
                  ε))))

(define (newton f f′ x ε)
  (cond
    ((< (abs (f x)) ε) x)
    (else (newton f
                  f′
                  (- x (/ (f x) (f′ x)))
                  ε))))


(define  (golden f a b ε)
  (define fi (/ (+ 1 (sqrt 5)) 2))
  
  (define (rec_golden f a b ε x1 x2)
    (let ((y1 (f x1))
          (y2 (f x2)))
      (cond
        ((< (abs(- b a)) ε) (/ (+ a b) 2))
        ((>= y1 y2) (rec_golden f x1 b ε x2 (+ x1 (/ (- b x1) fi))))
        (else (rec_golden f a x2 ε (- x2 (/ (- x2 a) fi)) x1)))))
  
  (let ((x1 (- b (/ (- b a) fi)))
        (x2 (+ a (/ (- b a) fi))))
    (rec_golden f a b ε x1 x2)))



(round
 (newton (lambda (x) (+ (* x x) (* 4 x) 4)) 
         (lambda (x) (+ (* 2 x) 4)) 
         5.0 1e-8))