#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define count-numbers
  (lambda (aexp)
    (cond
     ((atom? aexp) 1)
     ((null? (cdr aexp))
       (cond
	 ((atom? (car aexp)) 1)
	 (else (count-numbers (car aexp))))) 
     ((atom? (car aexp)) (+ 1 (count-numbers (cdr (cdr aexp)))))
     (else (+ (count-numbers (car aexp)) (+ 0 (count-numbers (cdr (cdr aexp)))))))))

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

(count-numbers aexp1)
(count-numbers aexp2)
(count-numbers aexp3)
(count-numbers aexp4)
