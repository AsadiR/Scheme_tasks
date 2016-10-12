(define (list->set xs)
  (define (rec rs xs)
    (cond ((null? xs) rs)
          ((member (car xs) rs) (rec rs (cdr xs)))
          (else (rec (cons (car xs) rs) (cdr xs)))))
  (rec '() xs))

(define (set? xs)
  (define (rec rs xs)
    (cond ((null? xs) #t)
          ((member (car xs) rs) #f)
          (else (rec (cons (car xs) rs) (cdr xs)))))
  (rec '() xs))

(define (set-eq? xs ys)
  ;все ли символы из xs есть в rs?
  (define (rec ys xs)
    (cond ((null? xs) #t)
          ((member (car xs) ys) (rec ys (cdr xs)))
          (else #f)))
  (and (rec xs ys) (rec ys xs)))

(define (union xs ys)
  (define (rec rs xs)
    (cond ((null? xs) rs)
          ((member (car xs) rs) (rec rs (cdr xs)))
          (else (rec (cons (car xs) rs) (cdr xs)))))
  (rec (rec '() xs) ys))

(define (intersection xs ys)
  (define (rec rs xs ys)
    (cond ((null? xs) rs)
          ((and (member (car xs) ys) (not (member (car xs) rs)))
           (rec (cons (car xs) rs) (cdr xs) ys))
          (else (rec rs (cdr xs) ys))))
  (rec (rec '() xs ys) ys xs))

(define (difference xs ys)
  (define (rec rs xs)
    (cond ((null? xs) rs)
          ((and (not (member (car xs) ys)) (not (member (car xs) rs)))
           (rec (cons (car xs) rs) (cdr xs)))
          (else (rec rs (cdr xs)))))
  (rec '() xs))

(define (symmetric-difference xs ys)
  (union (difference xs ys) (difference ys xs)))





(list->set '(1 1 2 3))
(set? '(1 2 3))
(set? '(1 2 2 3))
(set? '())
(set-eq? '(1 2 3) '(3 2 1))
(set-eq? '(1 2) '(1 3))
(union '(1 2 3) '(2 3 4))
(intersection '(1 2 3 7 4) '(3 5 4 7 9))
(difference '(1 2 3 4 5) '(2 3))
(symmetric-difference '(1 2 3 4) '(3 4 5 6))