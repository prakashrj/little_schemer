#lang racket

(define count0
  (lambda (vec)
    (cond
      ((null? vec) 0)
      ((zero? (car vec)) (+ 1 (count0 (cdr vec))))
      (else (count0 (cdr vec))))))

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

(define vec '(5 0 5 0))

(count0 vec)
