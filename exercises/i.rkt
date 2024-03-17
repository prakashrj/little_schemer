#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond 
     ((null? lat) #f)
     ((equal? a (car lat)) #t)
     (else (member? a (cdr lat))))))

(define g*
  (lambda (a b)
    (cond
      ((null? a) b)
      ((atom? (car a))
        (g* (cdr a) (+ (car a) b)))
       (else (g* (car a) (g* (cdr a) b))))))

(define l1 '((1 (6 6 ()))))
(define l2 '((1 2 (3 6)) 1))
(define l3 '())

(g* l1 l2)
