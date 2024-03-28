#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
	(and (numbered? (car aexp))
	     (numbered?
	       (car (cdr (cdr aexp)))))))))




(define aexp '(1 + 3))


(numbered? aexp)
