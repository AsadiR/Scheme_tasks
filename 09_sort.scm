
; сортировка прямым выбором

(define (get_elem pred? prev_min xs)
  (cond ((null? xs) #f)
        ((not (pred? (car xs) prev_min)) (car xs))
        (else (get_elem pred? prev_min (cdr xs)))))


(define (find_min pred? xs l prev_min cur_min)
  (cond ((null? xs) `(,l ,cur_min))
        ;equal prev
        ((and prev_min (equal? (car xs) prev_min )) (find_min pred? (cdr xs) (+ l 1) prev_min cur_min))
        ;already added
        ((and prev_min (pred? (car xs) prev_min)) (find_min pred? (cdr xs) l prev_min cur_min))
        ((and cur_min (pred? (car xs) cur_min)) (find_min pred? (cdr xs) l prev_min (car xs)))
        (else (find_min pred? (cdr xs) l prev_min cur_min))))

(define (repeat x l)
  (cond ((= l 0) '())
        (else (cons x (repeat x (- l 1))))))

(define (selection-sort pred? xs)
  (define cur_min (cadr (find_min pred? xs 0 #f (car xs))))
  (define (rec prev_min)
    (define cur_min (get_elem pred? prev_min xs))
    (cond ((not cur_min)
           (repeat prev_min (- (car (find_min pred? xs 0 prev_min #f)) 1) ))
          (else
           (let* ((l_and_min (find_min pred? xs 0 prev_min cur_min))
                  (l (- (car l_and_min) 1))
                  (min (cadr l_and_min)))
             `(,@(repeat prev_min l) ,min ,@(rec min))))))
  `(,cur_min ,@(rec cur_min)))




(define (insertion-sort pred? xs)
  (define (insert elem rs)
    (cond ((null? rs) (list elem))
          ((pred? (car rs) elem) `(,(car rs) ,@(insert elem (cdr rs))))
          (else `(,elem ,@rs))))
  
  (define (rec xs rs)
    (cond ((null? xs) rs)
          (else (rec (cdr xs) (insert (car xs) rs)))))
  
  (rec xs '()))





;(insert <= 4 '(1 3 5))

;(define (test1 x) x)


;(begin
;  (display "task1:\n")
;  (display (test1 1))
;  (newline))



; сортировка вставками
(insertion-sort <= '(9 6 2 5 9 4 3 5 7 1 8 8 0 0))
(selection-sort <= '(9 6 2 5 9 4 3 5 7 1 8 8 0 0))
(insertion-sort >= '(9 6 2 5 9 4 3 5 7 1 8 8 0 0))
(selection-sort >= '(9 6 2 5 9 4 3 5 7 1 8 8 0 0))



;(get_elem  <= 3 '(1 2 3 4 5 6))
;(get_elem  <= 6 '(1 2 3 4 5 6))

;(find_min <= '(1 2 1 2 3 4) 0 2 (get_elem  <= 2 '(1 2 1 2 3 4)))


;