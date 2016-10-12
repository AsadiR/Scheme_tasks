(define (bisection f a b e)
  (define (rec_bisection f a b e)
    (define mid (/ (+ a b) 2.0))
    (cond
      ((= 0 (f a)) a)
      ((= 0 (f b)) b)
      ((< (abs (f mid)) e) mid)
      (else (chose_section f a mid b e))
      ))
  
  (define (chose_section f a mid b e)
    (define inc_cond1 (and (< (f a) 0) (> (f mid) 0)))
    (define dec_cond1 (and (> (f a) 0) (< (f mid) 0)))
    (define inc_cond2 (and (< (f mid) 0) (> (f b) 0)))
    (define dec_cond2 (and (> (f mid) 0) (< (f b) 0)))   
    (cond
      ((or inc_cond1 dec_cond1) (rec_bisection f a mid e))
      ((or inc_cond2 dec_cond2) (rec_bisection f mid b e))
      (else "Function has no zero in this section!\n")))

  (rec_bisection f a b e))

(define (g x)
  (- (* x x) 4))

(begin
  (display (bisection g -10 10 0.01))
  (newline)
  (display (bisection cos -3.0 0.0 0.001))
  (newline))