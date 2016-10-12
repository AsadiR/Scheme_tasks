(load "unit_tests.scm")

(define (symbol-append . xs)
  (define first (symbol->string (car xs)))
  (define tail (cdr xs))
  (define (add_and_conv x)
    (string-append "-" (symbol->string x)))
  (string->symbol
   (string-append first (apply string-append (map add_and_conv tail)))))

(define (square? expr)
  (and (list? expr)
       (= (length expr) 3)
       (eq? (car expr) 'expt)
       (eq? (caddr expr) 2)))

(define (a-b? expr)
  (and (list? expr)
       (= (length expr) 3)
       (eq? (car expr) '-)))


(define (factorize expr)
  (and (a-b? expr)
       (let ((a (cadr expr))
             (b (caddr expr)))
         (and (square? a)
              (square? b)
              (let ((x (cadr a))
                    (y (cadr b)))
                (list '* (list '- x y)
                      (list '+ x y)))))))


(define p 5)
(define q 6)
(define expr-in '(- (expt p 2) (expt q 2)))
(define expr-out (factorize expr-in))


(begin
  ;(display (symbol-append 'ab 'cd 'ef))
  
  (write expr-out)(newline)

  (write (eval expr-out (interaction-environment)))
  (newline))
