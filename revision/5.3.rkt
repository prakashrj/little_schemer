#lang racket

(define multidown
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (cons (car lat) '()) (multidown (cdr lat)))))))

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

(multidown lat1)
(multidown lat2)
(multidown l4)
