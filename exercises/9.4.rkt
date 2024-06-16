#lang racket

(define Y
  (lambda (Ie)
    ((lambda (f) (f f))
(lambda (f)
(Ie (lambda (x) ((f f) x)))))))

(define rember
  (lambda (a l)
    ((Y (lambda (f)
	  (lambda (l)
	    (cond
	      ((equal? a (car l)) (cdr l))
	      (else (cons (car l) (f (cdr l) a))))))) a l)))

(define lat '(wad da hail))
(define a 'da)

(rember a lat)
