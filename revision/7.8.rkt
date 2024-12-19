#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lookup
  (lambda (var al)
    (cond
      ((null? al) 'nothing)
      ((equal? (car (car al)) var) (car (cdr (car al))))
      (else (lookup var (cdr al))))))

(define l1 '((x 1) (y 0)))
(define l2 '((u 1) (v 1)))
(define l3 '())
(define a 'y)
(define b 'u)

(lookup a l1) 
(lookup b l2) 
(lookup a l3)
