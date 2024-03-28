#lang racket

(define subst
 (lambda (new old lat)
  (cond
  ((null? lat) '())
  (else (cond ((eq? (car lat) old)
  (cons new (cdr lat)))
  (else (cons (car lat) (subst new old (cdr lat)))))))))

(define lat '(Hello my some is Nirekh))
(define new 'name)
(define old'some)

(subst new old lat)

