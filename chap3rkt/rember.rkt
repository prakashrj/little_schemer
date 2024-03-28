#lang racket

(define rember
 (lambda (a lat)
  (cond
  ((null? lat) '())
  ((eq? (car lat) a)
   (cdr lat))
  (else (cons (car lat) (rember a (cdr lat)))))))

(define lat'(bacon lettuce and tamato))
(define a'and)

(rember a lat)
