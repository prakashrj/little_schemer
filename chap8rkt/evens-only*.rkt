#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
	 ((even? (car l))
	  (cons (car l)
		(evens-only* (cdr l))))
	 (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
		  (evens-only* (cdr l)))))))

(define l '( 9 34 2))

(evens-only* l)
