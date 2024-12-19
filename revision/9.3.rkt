#lang racket

(define Y2
  (lambda (M)
    ((lambda (future)
       (M (lambda (arg1 arg2)
	    ((future future) arg1 arg2))))
     (lambda (future)
       (M (lambda (arg1 arg2)
	    ((future future) arg1 arg2)))))))

(define =
  (lambda (x y)
    ((Y2
    (lambda (f)
      (lambda (a b)
	(cond
	  ((and (zero? a) (zero? b)) #t)
	  ((or (zero? a) (zero? b)) #f)
	  (else (f (- a 1) (- b 1))))))) x y)))

(define pick
  (lambda (x y)
    ((Y2
       (lambda (f)
	 (lambda (a l)
	   (cond
	     ((zero? (- a 1) (car l))
	      (else (f (- a 1) (cdr l)))))))) x y)))

(define rempick
  (lambda (x y)
    ((Y2
       (lambda (f)
	 (lambda (n l)
	   (cond
	     ((zero? (- n 1)) (cdr l))
	      (else (cons (car l) (f (- n 1) (cdr l)))))))) x y)))
