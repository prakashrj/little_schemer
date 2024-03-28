#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member*
  (lambda (a l)
    (cond 
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a)
	   (member* a (cdr l))))
      (else (or (member* a (car l))
		(member* a (cdr l)))))))

(define a 'chips)
(define l'((potato) (chips ((with) fish) (chips))))

(member* a l)
