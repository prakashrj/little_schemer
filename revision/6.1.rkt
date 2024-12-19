#lang racket

(define atom?
(lambda (x)
(and (not (pair? x)) (not (null? x)))))

(define down*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))(cons (cons (car l) '()) (down* (cdr l))))
      (else (cons (down* (car l)) (down* (cdr l)))))))


(define l1 '((fried potatoes) (baked (fried)) tomatoes))
(define l2 '(((chili) chili (chili))))
(define l3 '())
(define lat1 '(chili and hot))
(define lat2 '(baked fried))
(define a 'fried)

(down* l2) 
(down* l3) 
(down* lat1)
