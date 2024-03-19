#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define count-op 
  (lambda (aexp)
    (cond
     ((atom? aexp) 0)
     ((null? (cdr aexp))
       (cond
	 ((atom? (car aexp)) 0)
	 (else (count-op (car aexp))))) 
     ((atom? (car aexp)) (+ 1 (count-op (cdr (cdr aexp)))))
     (else (+ (count-op (car aexp)) (+ 1 (count-op (cdr (cdr aexp)))))))))

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

(count-op aexp1)
(count-op aexp2)
(count-op aexp3)
(count-op aexp4)
