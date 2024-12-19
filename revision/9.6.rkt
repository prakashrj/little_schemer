#lang racket

(define accum
  (lambda (f v l)
      (cond
	((null? l) v)
	(else (f (car l) (accum f v (cdr l)))))))

(define multvec
  (lambda (l)
    (accum * 1 l)))

(define addvec
  (lambda (l)
    (accum + 0 l)))

(define mkn
  (lambda (l)
    (cond
      ((null? l) '())
       (else (cons 1 (cdr l))))))

(define length
  (lambda (l)
    (addvec (mkn l))))



(define vec1 '(1 2))
(define vec2 '(3 2 4))
(define vec3 '(2 1 3))
(define vec4 '(6 2 1))
(define l '())
(define obj '(x y))

 (length vec2)
 (length vec3)
 (length l)
