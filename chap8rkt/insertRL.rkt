#lang racket

(define insertRL
  (lambda (new oldR oldL lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldR)
       (cons new (cons oldR (insertRL new oldR oldL (cdr lat)))))
      ((eq? (car lat) oldL)
       (cons oldL (cons new (insertRL new oldR oldL (cdr lat)))))
      (else (cons
	      (car lat) (insertRL new oldR oldL (cdr lat)))))))


(define lat '(Hello Baka))
(define oldR 'Hello)
(define oldL 'Baka)
(define new 'Daddy)

(insertRL new oldR oldL lat)
      
       
