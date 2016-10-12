(define (day-of-week d m y)
  (define a (quotient (- 14 m) 12))
  (define (calculate_b y a) (- y a))
  (define (calculate_c m a) (+ m (* 12 a) -2))
  (define (calculate_day_of_week d b c)
    (define temp (+ 7000
                    (+ d
                       b
                       (quotient b 4)
                       (- (quotient b 100))
                       (quotient b 400)
                       (quotient (* 31 c) 12))))
    (remainder temp 7))
  (calculate_day_of_week d
                         (calculate_b y a)
                         (calculate_c m a)))
;a = (14 - m) / 12
;b = y − a
;c = m + 12 * a − 2
;day_of_week = (7000 + (d + b + b / 4 − b / 100 + b / 400 + (31 * c) / 12)) ОСТАТОК 7

(day-of-week 04 12 1975)
(day-of-week 04 12 2006)
(day-of-week 29 05 2013)
