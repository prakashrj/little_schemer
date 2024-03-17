#lang racket

(define vec1 '(1 2))
(define vec2 '(3 2 4))
(define vec3 '(2 1 3))
(define vec4 '(6 2 1))
(define l '())
(define obj '(x y))

(define <_
  (lambda (x y)
    (cond
    ((< x y) #t)
    ((= x y) #t)
    (else #f))))
(<_ 0 1)
(<_ 1 1)
(<_ 3 1)



