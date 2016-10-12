(define-syntax call
  (syntax-rules ()
    ((_ . body)
     (begin
       (define (func)
         (if #t (begin . body)))
       (func)))))


(call (display 3))
;полиморфный тип
;фигура = квадрат : a
;прямоуголник: a b
;треугольник a b c
;круг r