#lang racket

(define rember2
  (lambda (a l v)
    (cond
      ((null? l) '())
      ((eq? (car l) a)
       (cond
	 ((eq? v 1) (cdr l))
	 (else (cons a (rember2  a (cdr l) (+ v 1))))))
      (else (cons (car l) (rember2 a (cdr l) v))))))


(define l '(Elephant and Tiger and Lion and))
(define a 'and)
(define v 0)

(rember2 a l v)
