(define (string-trim-left s)
  (define l (string->list s))
  (define (rec l)
    (cond ((null? l) '())
          ((char-whitespace? (car l)) (rec (cdr l)))
          (else l)))
  (list->string (rec l)))

(define (reverse-str s)
  (define l (string->list s))
  (list->string (reverse l)))

(define (string-trim-right s)
  (reverse-str (string-trim-left (reverse-str s))))

(define (string-trim s)
  (reverse-str (string-trim-left (reverse-str (string-trim-left s)))))

(define (string-prefix? a b)
  (define al (string->list a))
  (define bl (string->list b))
  (define (rec a b)
    (cond ((and (null? a) (null? b)) #t)
          ((null? a) #t)
          ((null? b) #f)
          ((equal? (car a) (car b)) (rec (cdr a) (cdr b)))
          (else #f)))
  (rec al bl))

(define (string-suffix? a b)
  (string-prefix? (reverse-str a) (reverse-str b)))

(define (string-infix? a b)
  (define al (string->list a))
  (define bl (string->list b))
  (define (rec a b)
    (cond ((and (null? a) (null? b)) #t)
          ((null? a) #t)
          ((null? b) #f)
          ((string-prefix? (list->string a) (list->string b)) #t)
          (else (rec a (cdr b)))))
  (rec al bl))

(define (skip len s)
  (define (rec s len i)
    (cond ((= len i) s)
          (else (rec (cdr s) len (+ i 1)))))
  (rec s len 0))

(define (string-split str sep)
  (define strl (string->list str))
  (define sepl (string->list sep))
  (define len_of_sep (string-length sep))
  (define (rec strl word rs)
    (cond ((null? strl) (cons (list->string (reverse word)) rs))
          ((string-prefix? sep (list->string strl) )
           (rec (skip len_of_sep strl) '() (cons (list->string (reverse word)) rs)))
          (else (rec (cdr strl) (cons (car strl) word) rs))))
  (reverse (rec strl '() '())))



(string-trim-left "   abc")
(string-trim-right "abc def\t")
(string-trim       "\t abc def \n")

(string-prefix? "abc" "abcdef")
(string-prefix? "bcd" "abcdef")

(string-suffix? "def" "abcdef")
(string-suffix? "bcd" "abcdef")

(string-infix? "def" "abcdefgh")
(string-infix? "abc" "abcdefgh")
(string-infix? "fgh" "abcdefgh")
(string-infix? "ijk" "abcdefgh")

(string-split "x;y;z" ";")
(string-split "x1;y1;z1" ";")
(string-split "x y z" ";")


