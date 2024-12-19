#lang racket

(define <_
  (lambda (n1 n2)
    (cond
    ((zero? n1) #t)
    ((zero? n2) #f)
    (else (<_ (- n1 1) (- n2 1))))))

(define vec1 '(1 2))
(define vec2 '(3 2 4))
(define vec3 '(2 1 3))
(define vec4 '(6 2 1))
(define l '())
(define obj '(x y))

(<_ 4 8)
