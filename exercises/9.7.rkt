#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define count-op-f
  (lambda (op aexp)
      (cond
	((atom? aexp)
	 (cond ((eq? op aexp) 1) (else 0)))
	((null? (cdr aexp)) (count-op-f op (car aexp)))
	(else (+ (count-op-f op (car aexp)) (count-op-f op (cdr aexp)))))))
     


(define aexp1 '(1 + (3 * 4)))
(define aexp2 '((3 ↑ 4) + 5))
(define aexp3 '(3 * (4 * (5 * 6))))
(define aexp4 '5)
(define l1 '())
(define l2 '(3 + (66 6)))
(define lexp1 '(AND (OR x y) y))
(define lexp2 '(AND (OR x y) y))
(define lexp3 '(OR x y))
(define lexp4 'z)

(count-op-f '+ aexp1)
(count-op-f '+ aexp2)
(count-op-f '+ aexp3)
(count-op-f '+ aexp4)

