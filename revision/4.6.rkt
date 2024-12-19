#lang racket

(define product
  (lambda (vec1 vec2)
    (cond
      ((null? vec1) vec2)
      ((null? vec2) vec1)
      (else (cons (* (car vec1) (car vec2)) (product (cdr vec1) (cdr vec2)))))))

(define vec1 '(1 2))
(define vec2 '(3 2 4))
(define vec3 '(2 1 3))
(define vec4 '(6 2 1))
(define l '())
(define obj '(x y))

(product vec1 vec2)
(product vec2 vec3)
(product vec3 vec4)

