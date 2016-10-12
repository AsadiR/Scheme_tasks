(define-syntax when
  (syntax-rules ()
    (( _ cond? expr . exprs)
     (if cond?
         (begin expr . exprs)))))

(define-syntax unless
  (syntax-rules ()
    (( _ cond? expr . exprs)
     (if (not cond?)
         (begin expr . exprs)))))

(define-syntax for
  (syntax-rules (in as)
    ((for x in xs . body)
     (begin
       (define (func ls)
         (define x (car ls))
         (begin . body)
         (if (not (null? (cdr ls)))
             (func (cdr ls))))
       (func xs)))
    ((for xs as x . body) (for x in xs . body))))

(define-syntax while
  (syntax-rules ()
    ((while cond? . body)
     (begin
       (define (func)
         (begin . body)
         (if cond? (func)))
       (if cond? (func))))))

(define-syntax repeat
  (syntax-rules (until)
    ((repeat body until cond?)
     (begin
       (define (func)
         (begin . body)
         (if (not cond?) (func)))
       (func)))))

(define (convert x)
  (cond ((number? x) (number->string x))
        (else x)))

(define-syntax cout
  (syntax-rules (endl <<)
    ((cout << str . xs) (cout (convert str) xs))
    ((cout res (<< endl . xs))
     (cout (string-append res "\n") xs))                  
    ((cout res (<< str . xs))
     (cout (string-append res (convert str)) xs))
    ((cout res ())
     (begin
       (display res)))))
;(cout << "a=" << endl << "fgrf")
;(cout << "a = ")
(cout << "a = " << 1 << endl << "b = " << 2 << endl)



