#lang racket

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((equal? a (car lat) #t)
       (else (member? a (cdr lat)))))))



(define v '())

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
              ((eq? (car lat) a)
               (multirember a (cdr lat)))
            (else (cons (car lat)
                        (multirember a
                                     (cdr lat)))))))

(define reverselat
  (lambda (l v)
    (cond
      ((null? l) v)
      (else (reverselat (cdr l) (cons (car l) v))))))



(define symmetric?
  (lambda (rel)
    (cond
      ((null? rel) #t)
      ((member? (reverselat (car rel) '()) rel) (symmetric? (multirember  (car rel) (multirember (reverselat (car rel) '()) rel))))
      (else #f))))

(define antisymmetric?
  (lambda (rel)
    (cond
      ((null? rel) #t)
      ((equal? (car (car rel)) (car (cdr (car rel)))) (antisymmetric? (cdr rel)))
      ((member? (reverselat (car rel) '()) rel) #f)
      (else (antisymmetric? (multirember  (car rel) (multirember (reverselat (car rel) '()) rel)))))))


(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define symmetric?
  (lambda (alist)
    (cond
      ((null? alist) #t)
      ((equal? (car (car alist)) (car (cdr (car alist))))(symmetric? (cdr alist)))
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

(symmetric? r1)
(symmetric? r2) 
(symmetric? f2)
