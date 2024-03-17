#lang racket

(define dupla
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? (car lat) a) (cons (car lat) (dupla a (cdr lat))))
      (else 
	(cons a (dupla a (cdr lat)))))))

(define a 'hot)
(define lat '(texas hot chili))

(dupla a lat)
