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

(define f*
  (lambda (l acc)
    (cond
      ((null? l) acc)
      ((atom? (car l))
       (cond
	 ((member? (car l) acc) (f* (cdr l) acc))
	 (else (f* (cdr l) (cons (car l) acc)))))
      (else (f* (car l) (f* (cdr l) acc))))))


(define l1 '((8 2 7 4 1 0 7 8 9 (6 6 ()))))
(define l2 '((1 2 (3 6)) 1))
(define l3 '())

(f* l1 '(6 7 11))
(f* l2 '())
(f* l3 '())

