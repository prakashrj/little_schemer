#lang racket

(define remainder
  (lambda (n m)
    (- n (* m (/ n m)))))

(define build
  (lambda (s1 s2)
    (cond
      (else (cons s1
		  (cons s2 '())))))) 

(define str-maker
  (lambda (next n)
    (build n (lambda () (str-maker next (next
n))))))

(define int (str-maker add1 0))

(define frontier
  (lambda (str n)
    (cond
      ((zero? n) '())
      (else (cons (first str) (frontier (second str) (sub1 n)))))))

(define Q
  (lambda (str n)
    (cond
      ((zero? (remainder (first str) n))
       (Q (second str) n))
      (else (build (first str)
		   (lambda ()
		     (Q (second str) n)))))))

(define P
  (lambda (str)
    (build (first str) (lambda () (P (Q str (first str)))))))


(2 3 5
 
(frontier (P (build 7 (Q (build 8 (Q (str-maker add1 9) 3)) 2))) 7)

