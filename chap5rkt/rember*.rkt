#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember*
 (lambda (a l)
  (cond
  ((null? l) '())
  ((atom? (car l))
  (cond
   ((eq? (car l) a)
    (rember* a (cdr l)))
  (else (cons (car l)
	      (rember* a (cdr l))))))
  (else (cons (rember* a (car l))
	  (rember* a (cdr l)))))))

(define l '(((tomato sauce))
	   ((bean) sauce)
           (and ((flying)) sauce)))

(define a 'sauce)

(rember* a l)

