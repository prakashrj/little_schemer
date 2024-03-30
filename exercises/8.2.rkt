#lang racket

(define member?
  (lambda (a lat)
    (cond 
     ((null? lat) #f)
     ((equal? a (car lat)) #t)
     (else (member? a (cdr lat))))))

(define v '())

(define ab
  (lambda (al v)
    (cond
      ((null? al) v)
      ((equal? (car (car al)) (car (cdr (car al)))) (ab (cdr al) (cons (car (car al)) v)))
      (else (ab (cdr al) v)))))

(define reflexive?
  (lambda (al)
    (cond
      ((null? al) #t)
      ((member? (car (car al)) (ab al '()))
       (cond
	 ((member? (car (cdr (car al))) (ab al '())) (reflexive? (cdr al)))
	 (else #f)))
      (else #f))))

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

(reflexive? r1)
(reflexive? r2)
(reflexive? r3)


