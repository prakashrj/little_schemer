#lang racket

(define substN
  (lambda (new slat lat)
    (cond
      ((null? lat) '())
      ((member (car lat) slat) (cons new (cdr lat)))
      (else
	(cons (car lat) (substN new slat (cdr lat)))))))

(define new 'hot)
(define slat '(cincinnati chili))
(define lat '(texas hot chili))

(substN new slat lat)
