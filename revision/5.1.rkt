#lang racket
 
(define multisubst-kiwis
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat))(cons 'kiwis (multisubst-kiwis a (cdr lat))))
      (else (cons (car lat) (multisubst-kiwis a (cdr lat)))))))


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

(multisubst-kiwis b lat1)
(multisubst-kiwis y lat2)
(multisubst-kiwis y lat4)
(multisubst-kiwis y l4)
