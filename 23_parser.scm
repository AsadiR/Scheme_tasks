(define (char->str ch)
  (list->string (list ch)))

(define (char->sym ch)
  (string->symbol (char->str ch)))

(define (char->num ch)
  (- (char->integer ch) 48))

(define (list->sym l)
  (string->symbol (list->string l)))

(define (bracket? ch)
  (or (equal? ch #\( )
      (equal? ch #\) )))

(define (operation? ch)
  (or (equal? ch #\+ )
      (equal? ch #\- )
      (equal? ch #\* )
      (equal? ch #\/ )
      (equal? ch #\^ )))

(define (read-all seq)
  (define (rec head tail)
    (cond ((null? tail) (cons (reverse head) '()))
          ((char-alphabetic? (car tail))
           (rec (cons (car tail) head) (cdr tail)))
          (else (cons (reverse head) tail ))))
  (rec '() seq))


(define (tokenize seq)
  (define seql (string->list seq))
  (call-with-current-continuation
   (lambda (fail)
     (define (rec seql)
       (cond ((null? seql) '())
             ((bracket? (car seql))
              (cons (char->str (car seql)) (rec (cdr seql))))
             ((operation? (car seql))
              (cons (char->sym (car seql)) (rec (cdr seql))))
             ((char-whitespace? (car seql))
              (rec (cdr seql)))
             ((char-numeric? (car seql))
              (cons (char->num (car seql)) (rec (cdr seql))))
             ((char-alphabetic? (car seql))
              (let ((res (read-all seql)))
                (cons (list->sym (car res)) (rec (cdr res)))))
             (else (fail #f))))
     (rec seql))))

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

(define (parse seq)
  ;(display seq) (newline)
  (define src (make-source seq))
  (call-with-current-continuation
   (lambda (fail)
     ;Expr    ::= Term Expr' .
     (define (parse-expr)
       ;(display "parse-expr\n")
       (define pt (parse-term))
       (define pe_ (parse-expr_))
       (if (equal? pe_ 'void)
           pt
           (list pt pe_)))
     
     ;Expr'   ::= AddOp Term Expr' | .
     (define (parse-expr_)
       ;(display "parse-expr_\n")
       (cond ((equal? (peek src) '+)
              `(,(expected '+)
                ,(parse-term)
                ,(parse-expr_)))
             ((equal? (peek src) '-)
              `(,(expected '-)
                ,(parse-term)
                ,(parse-expr_)))
             (else 'void)))
     
     ;Term    ::= Factor Term' .
     (define (parse-term)
       ;(display "parse-term\n")
       (define pf (parse-factor))
       (define pt_ (parse-term_))
       (if (equal? pt_ 'void)
           pf
           (list pf pt_)))
     
     ;Term'   ::= MulOp Factor Term' | .
     (define (parse-term_)
       ;(display "parse-term_\n")
       (cond ((equal? (peek src) '*)
              `(,(expected '*)
                ,(parse-factor)
                ,(parse-term_)))
             ((equal? (peek src) '/)
              `(,(expected '/)
                ,(parse-factor)
                ,(parse-term_)))
             (else 'void)))
     
     ;Factor  ::= Power Factor' .
     (define (parse-factor)
       ;(display "parse-factor\n")
       (define pp (parse-pow))
       (define pf_ (parse-factor_))
       (if (equal? pf_ 'void)
           pp
           (list pp pf_)))
     
     
     ;Factor' ::= PowOp Power Factor' | .
     (define (parse-factor_)
       ;(display "parse-factor_\n")
       (cond ((equal? (peek src) '^)
              `(,(expected '^)
                ,(parse-pow)
                ,(parse-factor_)))
             (else 'void)))
     
     (define (value? x)
       (or (symbol? (peek src))
           (number? (peek src))))
     
     ;Power   ::= value | "(" Expr ")" | unaryMinus Power .
     (define (parse-pow)
       ;(display "parse-pow\n")
       (define sym (peek src))
       (cond ((equal? sym '-)
              `(,(expected '-)
                ,(parse-pow)))
             ((value? sym)
              (expected 'value))
             ((equal? "(" sym)
              `(,(expected "(")
                ,(parse-expr)
                ,(expected ")")))
             (else 'void)))
     
     (define (expected x)
       ;(display "sym: ")
       ;(display x)
       ;(newline)
       (define sym (next src))
       (cond ((equal? x 'value) sym)
             ((equal? x '^) 'expt)
             ((equal? x sym) sym)
             (else (fail 'error))))
     
     
     (let ((res (parse-expr))
           (sym (next src)))
       (if (equal?  #f sym)
           res
           (begin
             (display sym) 
             #f))
       ))))

(define (modify t)
  ;(b (/ c void)) -> (/ b c)
  `(,(caadr t) ,(car t) ,(cadadr t)))

(define (un_min? t)
  (and (list? t)
       (equal? (car t) '-)
       (= (length t) 2)))

(define (expt? t)
  (and (list? t)
       (equal? (car t) 'expt)
       (= (length t) 2)))


(define (tree->scheme t) 
  (cond ((equal? "(" (car t)) (tree->scheme (cadr t)))
        ((un_min? t) t)
        ((and (list? (car t))
              (list? (cadadr t)))
         `(,(caadr t)
           ,(tree->scheme (car t))
           ,(tree->scheme (cadadr t))))
        ((list? (car t))
         `(,(caadr t)
           ,(tree->scheme (car t))
           ,(cadadr t)))
        ((list? (cadadr t))
         `(,(caadr t)
           ,(car t)
           ,(tree->scheme (cadadr t))))
        (else
         `(,(caadr t)
           ,(car t)
           ,(cadadr t)))))




(define a 1)
(define b 2)
(define x 3)
(define y 4)
(define str "(-a + b) * (x^2 + y)")
(tokenize str)
(parse (tokenize str))
(tree->scheme (parse (tokenize str)))
(define expr (tree->scheme (parse (tokenize str))))
(eval expr (interaction-environment))


;(parse (tokenize "-a + b * x^2 + y"))
