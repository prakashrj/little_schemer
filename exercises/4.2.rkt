#lang racket

(define multivec
  (lambda (vec)
    (cond
      ((null? vec) 1)
      (else
	(* (car vec) (multivec (cdr vec)))))))

(define vec '(4 2 3))

(multivec vec)
