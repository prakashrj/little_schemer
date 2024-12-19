#lang racket

(define addvec
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ (car l) (addvec (cdr l)))))))

(define product
  (lambda (vec1 vec2)
    (cond
      ((null? vec1) vec2)
      ((null? vec2) vec1)
      (else (cons (* (car vec1) (car vec2)) (product (cdr vec1) (cdr vec2)))))))

(define dot-product
  (lambda (vec1 vec2)
    (addvec (product vec1 vec2))))

(define vec1 '(1 2))
(define vec2 '(3 2 4))
(define vec3 '(2 1 3))
(define vec4 '(6 2 1))
(define l '())
(define obj '(x y))

(dot-product vec2 vec2)
(dot-product vec2 vec4)
(dot-product vec3 vec4)

