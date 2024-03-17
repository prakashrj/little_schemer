#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define occurN*
  (lambda (lat l)
    (cond
      ((null? lat) 0)
      ((null? l) 0)
      ((atom? (car l)) 
        (cond 
	  ((member (car l) lat) (+ 1 (occurN* lat (cdr l))))
	  (else (occurN* lat (cdr l)))))
      (else (+ (occurN* lat (car l)) (occurN* lat (cdr l)))))))

(define l1 '((fried potatoes) (baked (fried)) tomatoes))
(define l2 '(((chili) chili (chili))))
(define l3 '())
(define lat1 '(chili and hot))
(define lat2 '(baked fried))
(define a 'fried)

(occurN* lat1 l2) 
(occurN* lat2 l1)
(occurN* lat1 l3)

