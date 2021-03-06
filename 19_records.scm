;(define-struct pos (row col))
;конструктора гет сет предикат


;(define (make-pos . xs) ...)
;(define (pos? instance) ...)
;(define )
;#(pos row col) - вектор для хранения структуры
;(define (set-pos-row! instance value) ...)
;(define (set-pos-col! instance value) ...)
(define (symbol-append . xs)
  (string->symbol (apply string-append (map symbol->string xs))))



(define (define-constructor% tag slots)
  (define constructor-name (symbol-append 'make- tag))
  (eval `(define (,constructor-name . xs)
           (list->vector (cons (quote ,tag) xs)))
        (interaction-environment)))

(define (define-getter% tag field index)
  (define getter-name (symbol-append tag '- field))
  (eval `(define (,getter-name x)
           (vector-ref x ,index))
        (interaction-environment)))

(define (define-setter% tag field index)
  (define setter-name (symbol-append 'set- tag '- field '!))
  (eval `(define (,setter-name x val)
           (vector-set! x ,index val))
        (interaction-environment)))



(define (define-predicate% tag)
  (define predicate-name (string->symbol (string-append (symbol->string tag) "?")))
  (eval `(define (,predicate-name x)
           (equal? (vector-ref x 0) (quote ,tag)))
        (interaction-environment)))

;(define (define-struct% tag slots)
;  (define-constructor% tag slots)
;  (define-type-pred%  ...)
;)

;и потом обернуть в макрос эти процедурки

;для алгебраического типа сходить на сайт rust-lang.org  ключевые слова struct match 

(define (define-struct% tag slots)
  (define-constructor% tag slots)
  (define-predicate% tag)
  (define (rec slots counter)
    (cond ((null? slots) '())
          (else
           (begin
             (define-getter% tag (car slots) counter)
             (define-setter% tag (car slots) counter)
             (rec (cdr slots) (+ counter 1))))))
  (rec slots 1))

(define-syntax define-struct
  (syntax-rules ()
    ((_ name fields) (define-struct% (quote name) (quote fields)))
    ))

;(define-struct% 'lol '(my_field))


;(define-constructor% 'lol '(my_field))
;(define-predicate% 'lol)
;(define-getter% 'lol 'my_field 1)
;(define-setter% 'lol 'my_field 1)

;(define my_lol (make-lol 1))
;(set-lol-my_field! my_lol 2)
;(display (lol? my_lol))
;(newline)
;(display my_lol)
;(newline)
;(display (lol-my_field my_lol))
;(newline)

;(begin . xs) => (begin (proc1) (proc2))

;(define (lol? x) (equal? (vector-ref x 0) 'lol))
;(lol? #(lol 1 2 3))

(define-struct% 'pos '(row col))
(define-struct pos (row col))
(define my_pos (make-pos 1 1))
(set-pos-row! my_pos 2)
(display (pos? my_pos))
(newline)
(display my_pos)
(newline)
(display (pos-row my_pos))
(newline)
(display (pos-col my_pos))
(newline)


;((lambda (x) x) 'lambda) 



