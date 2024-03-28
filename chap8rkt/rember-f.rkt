#lang racket

(define rember-f
  (lambda (test?)
    (lambda (a l)
    (cond
      ((null? l) '())
      ((test? (car l) a) (cdr l))
      (else (cons (car l)
		  ((rember-f test?) a (cdr l))))))))

(define a 'tuna)
(define l '(shrimp salad and tuna salad))

((rember-f eq?) a l)



