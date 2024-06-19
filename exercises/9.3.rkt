#lang racket

(define Y2
  (lambda (M)
    ((lambda (future)
       (M (lambda ( arg1 arg2)
	    ((future future) arg1 arg2))))
     (lambda (future)
       (M (lambda (arg1 arg2)
	    ((future future) arg1 arg2)))))))

(define =
  (lambda (n v)
    ((Y2 
      (lambda (f)
	(lambda (a b)
	  (cond
	    ((and (zero? a) (zero? b)) #t)
	    ((or (negative? a) (negative? b)) #f)
	    (else (f (- a 1) (- b 1))))))) n v)))

(define pick
  (lambda (n l)
    ((Y2
       (lambda (f)
	 (lambda (a b)
	   (cond
	     ((zero? (- a 1)) (car b))
	     (else (f (- a 1) (cdr b))))))) n l)))
			      
(define rempick
  (lambda (n l)
    ((Y2
       (lambda (f)
	 (lambda (a b)
	   (cond
	     ((zero? (- a 1)) (cdr b))
	     (else (cons (car b) (f (- a 1) (cdr b)))))))) n l)))

(define l '(wad da hail))
(define n 3)

(= 1 8)
(pick n l)
(rempick n l)

