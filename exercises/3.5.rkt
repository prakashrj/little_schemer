#lang racket

(define subst3
  (lambda (new o1 o2 o3 lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) o1) (cons new (cdr lat)))
      ((eq? (car lat) o2) (cons new (cdr lat)))
      ((eq? (car lat) o3) (cons new (cdr lat)))
    (else (cons (car lat) (subst3 new o1 o2 o3 (cdr lat)))))))

(define lat '(soy sauce and tomato sauce))
(define new 'soy)
(define o1 'chilli)
(define o2 'hot)
(define o3 'sauce)

(subst3 new o1 o2 o3 lat)
