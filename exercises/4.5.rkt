#lang racket 

(define index
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((equal? (car lat) a) 1)
    (else (+ 1 (index a (cdr lat)))))))


(define a 'car)
(define lat '(cons cdr car null? eq?))

(index a lat)

