#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define Fapply
  (lambda (f x)
    (cond
      ((null? f) 'noanswer)
      ((equal? (car (car f)) x)(car (cdr (car f))))
      (else (Fapply (cdr f) x)))))

(define member?
  (lambda (a alist)
    (cond
      ((null? alist) #f)
      ((equal? a (car (car alist))) #t)
      (else (member? a (cdr alist))))))

(define rmlast
  (lambda (l)
    (cond
      ((null? (cdr l)) '())
      (else (cons (car l) (rmlast (cdr l)))))))

(define last
  (lambda (l)
    (cond
      ((null? (cdr l))(car l))
      (else (last (cdr l))))))

(define cons2
  (lambda (l1 l2)
    ((null? l1) l2)
    (else
      (cons2 (rmlast l1) (cons (last l1) l2)))))

(define mk
  (lambda (xy z)
    (cond
      ((null? z) '())
      ((equal? (car (cdr xy)) (car (car z)))(cons (cons (car xy) (car (cdr (car z)))) (mk xy (cdr z))))
      (else (mk xy (cdr z))))))

(define Fcomp
  (lambda (f g)
    (cond
      ((null? g) '())
      ((member? (car (cdr (car g))) f)(cons2 (mk (car g) f) (Fcomp f (cdr g))))
      (else (Fcomp f (cdr g))))))

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

(Fcomp f1 f4) 
(Fcomp f1 f3) 
(Fcomp f4 f1)
(Fcomp f4 f3)
