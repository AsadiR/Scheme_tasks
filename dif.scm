(load "unit_tests.scm")

;predicate of expt
(define (expt? expr)
  (and (equal? (car expr) 'expt)
       (or (symbol? (cadr expr))
           (list? ))
       ))

;predicate of sum
(define (sum? expr)
  (and (list? expr)
       (equal? (car expr) '+)))

;predicate of minus
(define (minus? expr)
  (and (list? expr)
       (>= (length expr) 2)
       (equal? (car expr) '-)))

;predicate of unar minus
(define (un_minus? expr)
  (and (list? expr)
       (= (length expr) 2)
       (equal? (car expr) '-)))

;predicate of multiplication
(define (multi? expr)
  (and (list? expr)
       (equal? (car expr) '*)))

(define (gen_multi expr)
  (define args (cdr expr))
  (define (rec res first tail)
    (cond
      ((null? (cdr tail))
       (cons `(* ,@first ,(derivative (car tail))) res ))
      ((null? first)
       (rec (cons `(* ,(derivative (car tail)) ,@(cdr tail)) res)
         (cons (car tail) first)
         (cdr tail)))
      (else
       (rec (cons `(* ,@first ,(derivative (car tail)) ,@(cdr tail)) res)
         (cons (car tail) first)
         (cdr tail)))))
  (cond
    ((= (length expr) 2) (derivative (cadr expr)))
    (else `(+ ,@(rec '() '() args)))))

;predicate of dividing
(define (divide? expr)
  (and (list? expr)
       (equal? (car expr) '/)))

(define (gen_divide expr)
  (define first (cadr expr))
  (define tail (cddr expr))
  (define m_tail `(* ,@(cddr expr)))
  `(/ (- (* ,(derivative first) ,@tail) (* ,(derivative m_tail) ,first)) (* ,@tail ,@tail)))


;predicate of expt
(define (expt? expr)
  (and (list? expr)
       (equal? (car expr) 'expt)))


;x^y
(define (gen_expt expr)
  (define x (cadr expr))
  (define y (caddr expr))
  `(* (+ (* ,(derivative y) (log ,x)) (/ (* ,y  ,(derivative x)) ,x))
      (exp (* ,y (log ,x)))))


;predicate cos
(define (cos? expr)
  (and (list? expr)
       (equal? (car expr) 'cos)))


;predicate sin
(define (sin? expr)
  (and (list? expr)
       (equal? (car expr) 'sin)))


;predicate log
(define (log? expr)
  (and (list? expr)
       (equal? (car expr) 'log)))

;predicate exp
(define (exp? expr)
  (and (list? expr)
       (equal? (car expr) 'exp)))

;predicate of variable
(define (var? expr)
  (or (symbol? expr)
      (and (list? expr)
           (symbol? (car expr))
           (null? (cdr expr)))))



(define (derivative expr)
  (cond
    ((sum? expr) `(+ ,@(map derivative (cdr expr))))
    ((minus? expr) `(- ,@(map derivative (cdr expr))))
    ((multi? expr) (gen_multi expr))
    ((divide? expr) (gen_divide expr))
    ((expt? expr) (gen_expt expr))
    ((un_minus? expr) `(- ,@(derivative (cdr expr))))
    ((sin? expr) `(* ,(derivative (cadr expr)) (cos ,@(cdr expr))))
    ((cos? expr) `(- (* ,(derivative (cadr expr)) (sin ,@(cdr expr)))))
    ((log? expr) `(/ ,(derivative (cadr expr)) ,@(cdr expr)))
    ((exp? expr) `(* ,(derivative (cadr expr)) (exp ,@(cdr expr)))) 
    ((number? expr) 0)
    ((var? expr) 1)
    (else "end"))
  )

(define the-tests
  ;24
  (list (test (derivative '(+ x x)) '(+ 1 1))
        (test (derivative 2) 0)
        (test (derivative 'x) 1)
        (test (derivative '(- x)) '(- 1))
        (test (derivative '(* 1 x)) '(+ (* 1 1) (* 0 x)))
        (test (derivative '(* -1 x)) '(+ (* -1 1) (* 0 x)))
        (test (derivative '(* -4 x)) '(+ (* -4 1) (* 0 x)))
        (test (derivative '(* x y z)) '(+ (* y x 1) (* x 1 z) (* 1 y z)))
        (test (derivative '(* 10 x)) '(+ (* 10 1) (* 0 x)))
        (test (derivative '(- (* 2 x) 3)) '(- (+ (* 2 1) (* 0 x)) 0))
        (test (derivative '(/ 1 x)) '(/ (- (* 0 x) (* 1 1)) (* x x)))
        (test (derivative '(expt x 10)) '(* (+ (* 0 (log x)) (/ (* 10 1) x)) (exp (* 10 (log x)))))
        (test (derivative '(* 2 (expt x 5))) '(+ (* 2 (* (+ (* 0 (log x)) (/ (* 5 1) x)) (exp (* 5 (log x))))) (* 0 (expt x 5))))
        (test (derivative '(expt x -2)) '(* (+ (* 0 (log x)) (/ (* -2 1) x)) (exp (* -2 (log x)))))
        (test (derivative '(expt 5 x)) '(* (+ (* 1 (log 5)) (/ (* x 0) 5)) (exp (* x (log 5)))))
        (test (derivative '(cos x)) '(- (* 1 (sin x))))
        (test (derivative '(sin x)) '(* 1 (cos x)))
        (test (derivative '(exp x)) '(* 1 (exp x)))
        (test (derivative '(* 2 (exp x))) '(+ (* 2 (* 1 (exp x))) (* 0 (exp x))))
        (test (derivative '(* 2 (exp (* 2 x)))) '(+ (* 2 (* (+ (* 2 1) (* 0 x)) (exp (* 2 x)))) (* 0 (exp (* 2 x)))))
        (test (derivative '(log x)) '(/ 1 x))
        (test (derivative '(* 3 (log x))) '(+ (* 3 (/ 1 x)) (* 0 (log x))))  
        (test (derivative '(+ (expt x 3) (expt x 2)))
              '(+ (* (+ (* 0 (log x)) (/ (* 3 1) x)) (exp (* 3 (log x))))
                  (* (+ (* 0 (log x)) (/ (* 2 1) x)) (exp (* 2 (log x))))))
        (test (derivative '(- (* 2 (expt x 3)) (* 2 (expt x 2))))
              '(- (+ (* 2 (* (+ (* 0 (log x)) (/ (* 3 1) x)) (exp (* 3 (log x))))) (* 0 (expt x 3)))
                  (+ (* 2 (* (+ (* 0 (log x)) (/ (* 2 1) x)) (exp (* 2 (log x))))) (* 0 (expt x 2)))))
        (test (derivative '(/ 3 x)) '(/ (- (* 0 x) (* 1 3)) (* x x)))
        (test (derivative '(expt x x)) '(* (+ (* 1 (log x)) (/ (* x 1) x)) (exp (* x (log x)))))
        (test (derivative '(/ 1 (expt x x)))
              '(/ (- (* 0 (expt x x)) (* (* (+ (* 1 (log x)) (/ (* x 1) x)) (exp (* x (log x)))) 1))
                  (* (expt x x) (expt x x))))
        (test (derivative '(/ 3 (* 2 (expt x 2))))
              '(/ (- (* 0 (* 2 (expt x 2))) (* (+ (* 2 (* (+ (* 0 (log x)) (/ (* 2 1) x)) (exp (* 2 (log x))))) (* 0 (expt x 2))) 3))
                  (* (* 2 (expt x 2)) (* 2 (expt x 2)))))
        (test (derivative '(* 2 (sin x) (cos x)))
              '(+ (* (sin x) 2 (- (* 1 (sin x)))) (* 2 (* 1 (cos x)) (cos x)) (* 0 (sin x) (cos x))))
        (test (derivative '(sin (log (expt x 2))))
              '(* (/ (* (+ (* 0 (log x)) (/ (* 2 1) x)) (exp (* 2 (log x)))) (expt x 2))
                 (cos (log (expt x 2))))))
  )


(begin
  (run-tests the-tests)
  (newline))