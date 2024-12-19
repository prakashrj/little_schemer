#lang racket

(define /-
  (lambda (D d n)
    (cond
      ((< D d) n)
      (else (/- (- D d) d (+ n 1))))))

(define /
  (lambda (D d)
    (/- D d 0)))

(define vec1 '(1 2))
(define vec2 '(3 2 4))
(define vec3 '(2 1 3))
(define vec4 '(6 2 1))
(define l '())
(define obj '(x y))

(/ 5 4)

