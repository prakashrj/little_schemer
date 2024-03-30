#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond 
     ((null? lat) #f)
     ((equal? a (car lat)) #t)
     (else (member? a (cdr lat))))))

(define v '())

(define null2
  (lambda (S) 
    (cond
      ((null? S) #t)
      ((atom? S) #f)
      (else (and (null2 (car S)) (null2 (cdr S)))))))

(define 1st-atom
  (lambda (S)
    (cond
      ((atom? S) S)
      ((atom? (car S)) (car S))
      ((null2 (car S)) (1st-atom (cdr S)))
      (else (1st-atom (car S)))))) 

(define rmvatom 
  (lambda (a l) 
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
	 ((equal? a (car l)) (rmvatom a (cdr l)))
	 (else (cons (car l) (rmvatom a (cdr l))))))
      (else (cons (rmvatom a (car l)) (rmvatom a (cdr l)))))))

(define domset
  (lambda (rel v)
    (cond 
      ((null2 rel) v)
      (else (domset (rmvatom (1st-atom rel) rel) (cons (1st-atom rel) v))))))


(define idrel
  (lambda (set)
    (cond
      ((null? set) '())
      (else (cons (cons (car set) (cons (car set) '())) (idrel (cdr set)))))))
        

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

(domset r1 '())
(idrel d1)




