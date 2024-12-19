#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define logic?
  (lambda (a)
    (cond
      ((equal? a 'AND) #t)
      ((equal? a 'OR) #t)
      (else #f))))
(define NOT?
  (lambda (a)
    (cond
      ((equal? a 'NOT) #t)
      (else #f))))

(define lexp?1
  (lambda (l)
    (cond
      ((null? l) #t)
      ((and (atom? l) (not (number? l))) #t)
      ((logic? (car l))(and (lexp?1 (car (cdr l))) (lexp?1 (car (cdr (cdr l))))))
      ((NOT? (car l))(lexp?1 (car (cdr l))))
      (else #f))))

(define lexp?
  (lambda (l)
    (and (lexp?1 l) (not (null? l)))))


(define aexp1 '(1 + (3 * 4)))
(define aexp2 '((3 ^ 4) + 5))
(define aexp3 '(3 * (4 * (5 * 6))))
(define aexp4 5)
(define l1 '())
(define l2 '(3 + (66 6)))
(define lexp1 '(AND (OR x y) y))
(define lexp2 '(AND (NOT y) (OR u v)))
(define lexp3 '(OR x y))
(define lexp4 'z)

(lexp? lexp1)
(lexp? lexp2)
(lexp? lexp3)
(lexp? aexp1)
(lexp? l2)
