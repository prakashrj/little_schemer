#lang racket

(define occurn
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((equal? a (car lat))(+ 1 (occurn a (cdr lat))))
      (else (occurn a (cdr lat))))))

(define occurN
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) 0)
      (else (+ (occurn (car lat1) lat2) (occurN (cdr lat1) lat2))))))

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
