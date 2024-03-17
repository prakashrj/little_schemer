#lang racket

(define occurN
  (lambda (l1 l2)
    (cond
      ((null? l1) 0)
      ((null? l2) 0)
     ((member (car l1) l2) (+ 1 (occurN (cdr l1) l2)))
     (else (occurN (cdr l1) l2)))))

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

(occurN lat1 l4)
(occurN lat1 lat2)
(occurN lat1 lat3)
