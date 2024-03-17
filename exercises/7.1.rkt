#lang racket

(define mk+exp
  (lambda (aexp1 aexp2)
    (cons aexp1
	  (cons (quote +)
		(cons aexp2 ( ))))))

(define mk*exp
  (lambda (aexp1 aexp2)
    (cons aexp1
	  (cons (quote *)
		(cons aexp2 ( ))))))

(define mk↑exp
  (lambda (aexp1 aexp2)
    (cons aexp1
	  (cons (quote ↑)
		(cons aexp2 ( ))))))

(define aexp1 '(1 +(3 * 4)))
(define aexp2 '((3 ↑ 4) + 5))
(define aexp3 '(3 * (4 * (5 * 6))))
(define aexp4 ' 5)
(define l1 '())
(define l2 '(3 + (66 6)))
(define lexp1 (AND (OR x y) y))
(define lexp2 (AND (OR x y) y))
(define lexp3 '(OR x y))
(define lexp 'z)
