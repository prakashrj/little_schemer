#lang racket

(define I
  (lambda (lat1 lat2)
    (cond
      ((null? lat2) '())
     ((member (car lat2) lat1) (car lat2))
     (else (I lat1 (cdr lat2))))))


(define multiI
  (lambda (lat1 lat2)
    (cond
      ((null? lat2) '())
     ((member (car lat2) lat1) (cons (car lat2) (multiI lat1 (cdr lat2))))
     (else (multiI lat1 (cdr lat2))))))

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

(I lat1 l4)
(I lat1 lat2)
(I lat1 lat3)
(multiI lat1 l4)
(multiI lat1 lat2)
(multiI lat1 lat3)
