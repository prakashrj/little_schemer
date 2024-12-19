#lang racket

(define duplicate
  (lambda (n obj)
    (cond
      ((zero? n) '())
      (else (cons obj (duplicate (- n 1) obj))))))

(define vec1 '(1 2))
(define vec2 '(3 2 4))
(define vec3 '(2 1 3))
(define vec4 '(6 1 2))
(define l '())
(define obj '(x y))

(duplicate 3 obj)
(duplicate 0 obj)
(duplicate 1 vec1)

