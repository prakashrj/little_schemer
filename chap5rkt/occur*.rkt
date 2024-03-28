#lang racket

(define occur*
 (lambda (a l)
   (cond
((null? l) 0)
((atom? (car l))
 (cond 
  ((eq? (car l) a)
   (+ 1 (occur* a (cdr l))))
  (else (occur* a (cdr l)))))

(else (+ (occur* a (car l))
	 (occur* a (cdr l)))))))

(define atom?
 (lambda (x)
  (and (not (pair? x)) (not (null? x)))))

(define a 'banana)

(define l '((banana)
	    (split ((((banana ice)))
		    (cream (banana))
		    sherbet))
	    (banana)
	    (bread)
	    (banana brandy)))

(occur* a l)



 
