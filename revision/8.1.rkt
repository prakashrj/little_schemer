#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define firsta
  (lambda (alist)
    (cond
      ((null? alist) '())
      ((atom? (car alist)) (car alist))
      ((null? (car alist))(firsta (cdr alist)))
      (else (firsta (car alist))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? (car lat) a)(multirember a (cdr lat)))
       (else (cons (car lat) (multirember a (cdr lat)))))))

(define multirember*
  (lambda (a alist)
    (cond
      ((null? alist) '())
      (else (cons (multirember a (car alist)) (multirember* a (cdr alist)))))))

(define domset
  (lambda (r)
    (cond
      ((null? r) '())
      ((null? (car r))(domset (cdr r)))
      (else (cons (firsta r) (domset (multirember* (firsta r) r)))))))

(define idrel
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (cons (car lat) (cons (car lat) '())) (idrel (cdr lat)))))))
			     

(define r1 '((a b) (a a) (b b)))
(define r2 '((c c)))
(define r3 '((a c) (b c)))
(define r4 '((a b) (b a)))
(define f1 '((a 1) (b 2) (c 2) (d 1)))
(define f2 '())
(define f3 '((a 2) (b 1)))
(define f4 '((1 $) (3 *)))
(define d1 '(a b))
(define d2 '(c d))
(define x 'a)

(domset r1)
(domset r2)
(domset r3) 

(idrel d1) 
(idrel d2)
(idrel f2)

