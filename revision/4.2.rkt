#lang racket

(define multvec
  (lambda (l)
    (cond
      ((null? l) 1)
      (else (* (car l) (multvec (cdr l)))))))

(define vec1 '(1 2))
(define vec2 '(3 2 4))
(define vec3 '(2 1 3))
(define vec4 '(6 2 1))
(define l '())
(define obj '(x y))

 (multvec vec2)
 (multvec vec3)
 (multvec l)
