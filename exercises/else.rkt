#lang racket

(define n 2)
(define obj 'Hello)

(define duplicate
  (lambda (n obj)
    (cond
    ((zero? n) '())
 (else (cons obj (duplicate (- n 1) obj))))))

(duplicate n obj) 


(define duplicate2
  (lambda (n obj)
    (cond
    ((zero? n) '())
    (+ 1)
    )))

(duplicate2 n obj) 

(define duplicate3
  (lambda (n obj)
    (cond
    ((zero? n) '())
    (cons obj)
    )))

(duplicate3 n obj) 

(define duplicate4
  (lambda (n obj)
    (cond
    ((zero? n) '())
    (else 1)
    )))

(duplicate4 n obj) 

(define duplicate5
  (lambda (n obj)
    (cond
    ((zero? n) '())
    (else obj)
    )))

(duplicate5 n obj) 

(define duplicate7
  (lambda (n obj)
    (cond
    ((zero? n) '())
    (1 1)
    )))

(duplicate7 n obj) 


(define duplicate8
  (lambda (n obj)
    (cond
    ((zero? n) '())
    (obj obj)
    )))

(duplicate8 n obj) 


(define duplicate6
  (lambda (n obj)
    (cond
    ((zero? n) '())
    (#t 'true)
    )))

(duplicate6 n obj) 


(define duplicate9
  (lambda (n obj)
    (cond
    ((zero? n) '())
    (#f 'false)
    )))

(duplicate9 n obj) 


(define duplicate10
  (lambda (n obj)
    (cond
    ((zero? n) '())
    ((void) 'void)
    )))

(duplicate10 n obj) 


