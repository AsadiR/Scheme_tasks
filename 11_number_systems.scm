
(define (normilize x)
  (cond ((char-numeric? x) (- (char->integer x) (char->integer #\0)))
        ((and (char-alphabetic? x) (char-lower-case? x))
         (- (char->integer x) 87))
        ((and (char-alphabetic? x) (char-upper-case? x))
         (- (char->integer x) 55))
        (else 40)))

(define (unnormolize x)
  (cond ((< x 10) (integer->char (+ x (char->integer #\0))))
        ((and (>= x 10) (<= x 36)) (integer->char (+ x 87)))
        (else 'error)))

(define (find_elem_ge b xs)
  (cond ((null? xs) #f)
        ((>= (car xs) b) #t)
        (else (find_elem_ge b (cdr xs)))))


(define (certain->decimal s b)
  (define xs (map normilize (string->list s)))
  
  (define (rec xs k)
    (cond ((null? xs) 0)
          (else (+ (* (car xs) (expt b k)) (rec (cdr xs) (- k 1))))))
  
  (if (find_elem_ge b xs)
      'number-conversion-error
      (rec xs (- (length xs) 1))))

(define (correct-number? s b)
  (define xs (map normilize (string->list s)))
  (if (find_elem_ge b xs) #f #t))



(define (decimal->certain d b)
  (define (rec d b res)
    (define rem (remainder d b))
    (define quot (quotient d b))
    (cond ((< quot b) (cons quot (cons rem res)))
          (else (rec quot b (cons rem res)))))

  (list->string (map unnormolize (rec d b '() ))))






;(certain->decimal? s b)
;(decimal->certain d b)

;111 (2) -> 1*2^2 + 1*2^1 + 1*2^0 = 7
;y....x (n) -> y*n^(l-1) +.....+ x*n^0
;abcdefghijklmnopqrstuvwxyz = 26
;итого всего 10 + 26 символов => максимально поддерживаемая система счисления 36

;(certain->decimal "111" 2)
;(string->number "1")

;(begin (define (to-downcase s) (list->string (map char-downcase (string->list s)))) (to-downcase (decimal->certain 479 8)))

(certain->decimal "ZZ" 36)
(certain->decimal "FF" 16)
(certain->decimal "ZZ" 35)
(correct-number? "ab0" 12)
(correct-number? "ab0" 10)
(decimal->certain 255 16)
(decimal->certain 1295 36)
(decimal->certain 479 8)

