#lang racket

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define l '(((hot) (tuna (and))) cheese))

(leftmost l)
