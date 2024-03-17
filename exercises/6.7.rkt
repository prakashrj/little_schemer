#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define g*
  (lambda (lvec acc)
    (cond
      ((null? lvec) acc)
      ((atom? (car lvec))
       (g* (cdr lvec) (+ (car lvec) acc )))
      (else (g* (car lvec) (g* (cdr lvec) acc))))))

(define l1 '((1 (6 6 ()))))
(define l2 '((1 2 (3 6)) 1))
(define l3 '())



