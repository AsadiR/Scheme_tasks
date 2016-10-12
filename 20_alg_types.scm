;(define-syntax test
;  (syntax-rules ()
;    ((_)(define a 4))
;    ))

;(test)
;(display a)


(define (symbol-append . xs)
  (string->symbol (apply string-append (map symbol->string xs))))



(define (define-constructor% outer_tag tag slots)
  (define constructor-name (symbol-append tag))
  (eval `(define (,constructor-name . xs)
           (cons (quote ,outer_tag) (list->vector (cons (quote ,tag) xs))))
        (interaction-environment)))

(define (define-getter% tag field index)
  (define getter-name (symbol-append tag '- field))
  (eval `(define (,getter-name x)
           (vector-ref (cdr x) ,index))
        (interaction-environment)))

(define (define-setter% tag field index)
  (define setter-name (symbol-append 'set- tag '- field '!))
  (eval `(define (,setter-name x val)
           (vector-set! (cdr x) ,index val))
        (interaction-environment)))



(define (define-predicate% outer_tag tag)
  (define predicate-name (symbol-append tag '?))
  (eval `(define (,predicate-name x)
           (and (equal? (vector-ref (cdr x) 0) (quote ,tag))
                (equal? (car x) (quote ,outer_tag))))
        (interaction-environment)))

(define (define-struct% outer_tag tag slots)
  (define-constructor% outer_tag tag slots)
  (define-predicate% outer_tag tag)
  (define (rec slots counter)
    (cond ((null? slots) '())
          (else
           (begin
             (define-getter% tag (car slots) counter)
             (define-setter% tag (car slots) counter)
             (rec (cdr slots) (+ counter 1))))))
  (rec slots 1))


(define (define-data% outer_tag types)
  (define predicate-name (symbol-append outer_tag '?))
  (eval `(define (,predicate-name x)
           (equal? (car x) (quote ,outer_tag)))
        (interaction-environment))
  (define (rec types)
    ;(display types)
    ;(newline)
    (cond ((null? types) '())
          (else (begin
                  (define-struct% outer_tag (caar types) (cdar types))
                  (rec (cdr types))))))
  (rec types))


(define-syntax define-data
  (syntax-rules ()
    ((_ outer_tag types) (define-data% (quote outer_tag) (quote types)))
    ))

;;;;;;;;
(define (create-body% elem args body)
  ;elem - элемент из которого дергаем поля
  ;args - переменные которые нужно объявить
  ;body - исполняемое действие
  (define vec (cdr elem))
  (define (rec args counter)
    (cond ((null? args) '())
          (else
           (cons `(,(car args) ,(vector-ref vec counter))
                 (rec (cdr args) (+ 1 counter))))))
  `(let ,(rec (cdr args) 1) ,body))



(define (match% elem xs)
  (define elem-name (vector-ref (cdr elem) 0))
  (define (rec xs)
    (cond ((null? xs) 'error)
          (else
           (if (equal? (caaar xs) elem-name)
               (create-body% elem (caar xs) (cadar xs)) 
               (rec (cdr xs))))))
  ;smth strange
  (display (rec xs))
  (newline)
  (rec xs))

(define-syntax match
  (syntax-rules ()
    ((_ elem . xs) (eval (match% elem (quote xs)) (interaction-environment)))
    ))



(define-data% 'figure '((square a)
                        (rectangle a b)
                        (triangle a b c)
                        (circle r)))


;(display fig1)
;(newline)
;(display (square-a fig1))

;(newline)
;(create-body% '(figure . #(square 2)) '(a) '(* a a))
;(newline)
;(create-body% '(figure . #(rectangle 2 3)) '(a b) '(* a b))

(define pi (acos -1))

(define (perim fig)
  (match fig
    ((square a)       (* 4 a))
    ((rectangle a b)  (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r)       (* 2 pi r))))


(define s (square 10))
(define r (rectangle 10 20))
(define t (triangle 10 20 30))
(define c (circle 10))

(perim s)
(perim r)
;(display c)
(perim t)
;(display c)
(perim c) 
