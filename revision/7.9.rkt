#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define convert01
  (lambda (a)
    (cond
      ((zero? a) #f)
      (else #t))))

(define lookup
  (lambda (var al)
    (cond
      ((null? al) 'nothing)
      ((equal? (car (car al)) var) (car (cdr (car al))))
      (else (lookup var (cdr al))))))

(define Mlexp
  (lambda (lexp al)
    (cond
      ((atom? lexp)(convert01 (lookup lexp al)))
      ((equal? (car lexp) 'NOT)(not (Mlexp (car (cdr lexp)) al)))
      ((equal? (car lexp) 'OR)(or (Mlexp (car (cdr lexp)) al) (Mlexp (car (cdr (cdr lexp))) al)))
      (else (and (Mlexp (car (cdr lexp)) al) (Mlexp (car (cdr (cdr lexp))) al))))))

(define l1 `((x 1) (y 0) (z 0)))
(define l2 '((y 0) (u 0) (v 1)))
(define lexp1 '(AND (OR x y) y))
(define lexp2 '(AND (NOT y) (OR u v)))
(define lexp4 'z)

(Mlexp lexp1 l1) 
(Mlexp lexp2 l2) 
(Mlexp lexp4 l1)
