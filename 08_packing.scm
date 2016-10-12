(define (pack xs)
  (define (rec word xs rs)
    (cond ((null? xs) (cons word rs))
          ((null? word) (rec (cons (car xs) '()) (cdr xs) rs))
          ((equal? (car word) (car xs)) (rec (cons (car xs) word) (cdr xs) rs))
          (else (rec (cons (car xs) '()) (cdr xs) (cons word rs)))))
  (reverse (rec '() xs '())))

(define (encode xs)
  (define pxs (pack xs))
  (define (rec pxs)
    (cond ((null? pxs) '())
          (else (cons `( ,(car (car pxs)) ,(length (car pxs)))
                      (rec (cdr pxs))))))
  (rec pxs))

(define (repeat xs)
  (define l (cadr xs))
  (define sym (car xs))
  (define (rec i)
    (cond ((= l i) '())
          (else (cons sym (rec (+ i 1))))))
  (rec 0))
  

   
(define (unpack xs)
  (cond ((null? xs) '())
        (else `( ,(repeat (car xs)) ,@(unpack (cdr xs))))))

(define (decode xs)
    (cond ((null? xs) '())
        (else `( ,@(repeat (car xs)) ,@(decode (cdr xs))))))

(pack '(a a a b b c))
(encode '(a a a b b c))
(unpack '((a 3) (b 2) (c 1)))
(decode '((a 3) (b 2) (c 1))) 
