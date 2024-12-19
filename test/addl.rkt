#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define addl
  (lambda (l)
    (cond
      ((null? l) 0)
      ((atom? l)
       (cond
	 ((number? l) l)
	 (else 0)))
      (else (+ (addl (car l)) (addl (cdr l)))))))

(addl '(9 (10 9)))
