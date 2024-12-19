#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define searchn
  (lambda (n l)
    (cond
      ((null? l) #f)
      ((equal? n (car l)) n)
      (else (searchn n (cdr l))))))

(searchn 1 '(1 2 3))
(searchn 2 '(a b d))



