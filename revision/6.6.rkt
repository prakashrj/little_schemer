#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define list+
  (lambda (l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) (+ (car l) (list+ (cdr l))))
      (else (+ (list+ (car l)) (list+ (cdr l)))))))

(define l1 '((1 (6 6 ()))))
(define l2 '((1 2 (3 6)) 1))
(define l3 '())

(list+ l1) 
(list+ l2) 
(list+ l3)
