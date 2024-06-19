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
	      (else (cons (car l) (f (cdr l)))))))) l)))

(define insertR
  (lambda (new old lat)
    ((Y (lambda (f)
	  (lambda (lat)
	    (cond
	      ((null? lat) '())
	      ((equal? (car lat) old) (cons old (cons new (cdr lat))))
	      (else (cons (car lat) (f (cdr lat)))))))) lat)))

(define subst2
  (lambda (new o1 o2 lat)
    ((Y (lambda (f)
	  (lambda (lat)
	    (cond
	      ((null? lat) '())
	      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
	      (else (cons (car lat) (f (cdr lat)))))))) lat)))



(define lat '(wad da hail))
(define a 'da)
(define new 'sigma)
(define old 'da)
(define b 'wad)

(rember a lat)
(insertR new old lat)
(subst2 new a b lat)
