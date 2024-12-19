#lang racket

(define i
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat))(car lat))
      (else (i a (cdr lat))))))

(define I
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) '())
      ((not (null? (i (car lat1) lat2)))(i (car lat1) lat2))
      (else (I (cdr lat1) lat2)))))

(define multiI
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) '())
      ((not (null? (i (car lat1) lat2)))(cons (i (car lat1) lat2) (multiI (cdr lat1) lat2)))
      (else (multiI (cdr lat1) lat2)))))

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
