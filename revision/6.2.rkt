#lang racket

(define atom?
(lambda (x)
(and (not (pair? x)) (not (null? x)))))

(define occurn*
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((not (atom? (car lat)))(+ (occurn* a (car lat)) (occurn* a (cdr lat))))
      ((equal? a (car lat))(+ 1 (occurn* a (cdr lat))))
      (else (occurn* a (cdr lat))))))


(define occurN*
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) 0)
      (else (+ (occurn* (car lat1) lat2) (occurN* (cdr lat1) lat2))))))

(define l1 '((fried potatoes) (baked (fried)) tomatoes))
(define l2 '(((chili) chili (chili))))
(define l3 '())
(define lat1 '(chili and hot))
(define lat2 '(baked fried))
(define a 'fried)


(occurN* lat1 l2)
(occurN* lat2 l1)
(occurN* lat1 l3)
