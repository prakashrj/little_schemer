#lang racket

(define cnt-aexp
  (lambda (aexp)
    (cond
      ((null? (cdr aexp)) 0)
      (else (+ 1 (cnt-aexp (cdr aexp)))))))

(define ^
  (lambda (n1 n2)
    (cond
      ((zero? n2) 1)
      (else (* n1 (^ n1 (- n2 1)))))))

(define value
  (lambda (aexp)
    (cond 
      ((number? aexp) aexp)
    (else (define v (cnt-aexp aexp))
    (cond
      ((null? aexp) 0)
      ((eq? v 2)
       (cond
	 ((equal? (car aexp) '+)
	  (+ (value (car (cdr aexp))) (value (car (cdr (cdr aexp))))))
	 ((equal? (car aexp) '*)
	  (* (value (car (cdr aexp))) (value (car (cdr (cdr aexp))))))
	 (else
	  (^ (value (car (cdr aexp))) (value (car (cdr (cdr aexp))))))))
      ((eq? v 3)
       (cond
	 ((equal? (car aexp) '+)
	(+ (+ (value (car (cdr aexp))) (value (car (cdr (cdr aexp))))) (value (car (cdr (cdr (cdr aexp)))))))
	 (else
	(* (* (value (car (cdr aexp))) (value (car (cdr (cdr aexp))))) (value (car (cdr (cdr (cdr aexp)))))))))
      (else
	(cond
	  ((equal? (car aexp) '+)
	(+ (+  (+ (value (car (cdr aexp))) (value (car (cdr (cdr aexp))))) (value (car (cdr (cdr (cdr aexp)))))) (value (car (cdr (cdr (cdr (cdr aexp))))))))
	(else
	(* (*  (* (value (car (cdr aexp))) (value (car (cdr (cdr aexp))))) (value (car (cdr (cdr (cdr aexp)))))) (value (car (cdr (cdr (cdr (cdr aexp)))))))))))))))

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

(value '(+ (* 3 2) 3 5))
