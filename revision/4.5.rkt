#lang racket

   (define index-
    (lambda (a lat n)
     (cond
       ((null? lat) #f)
       ((equal? a (car lat)) n)
       (else (index- a (cdr lat) (+ n 1))))))

(define index
  (lambda (a lat)
    (index- a lat 1)))

(define vec1 '(1 2))
(define vec2 '(3 2 4))
(define vec3 '(2 1 3))
(define vec4 '(6 2 1))
(define l '())
(define obj '(x y))

(define lat1 '(cons cdr car null? eq?))
(define lat2 '(car engine auto motor))
(define a 'car)
(define b 'motor)

(index a lat1 )
(index a lat2 )
(index a '())
(index b lat2 )


