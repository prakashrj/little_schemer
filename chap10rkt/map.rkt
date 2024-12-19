#lang racket

(define map
  (lambda (f lat)
    (cond
      ((null? lat) '())
      (else (cons (f (car lat)) (map f (cdr lat)))))))

(define add1
  (lambda (n)
    (+ 1 n)))


(map number? '(1 a b 3 s 5))
(map add1 '(1 3 5))

