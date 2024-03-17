#lang racket

(define multisubst2
  (lambda (x a1 a2 lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a1) (cons x (multisubst2 x a1 a2 (cdr lat))))
       ((eq? (car lat) a2) (cons x (multisubst2 x a1 a2 (cdr lat))))
       (else (cons (car lat) (multisubst2 x a1 a2 (cdr lat)))))))

(define x 'comma)
(define y 'dot)
(define a 'kiwis)
(define b 'plums)
(define lat1 '(bananas kiwis))
(define lat2 '(peaches apples bananas))
(define lat3 '(kiwis pears plums bananas cherries))
(define lat4 '(kiwis mangoes kiwis guavas kiwis))
(define l1 '((curry) ( ) (chicken) ( )))
(define l2 '((peaches) (and cream)))
(define l3 '((plums) and (ice) and cream))
(define l4 '())

(multisubst2 x a b lat1)
(multisubst2 y a b lat3)
(multisubst2 a x y lat1)
