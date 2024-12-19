#lang racket

(define eternity
  (lambda (l)
    (eternity l)))

(define C
  (lambda (n)
    (cond
      ((= 1 n) 1)
      (else
	(cond
	  ((even? n) (C (/ n 2)))
	  (else (C (+ 1 (* 3 n))))))))) 

(define A
  (lambda (n m)
    (cond
      ((zero? n) (+ 1 m))
      ((zero? m) (A (- n 1) 1))
      (else (A (- n 1) (A n (- m 1)))))))

((lambda (mk-length)
   (mk-length mk-length))
(lambda (mk-length)
  ((lambda (length)
     (lambda (l)
       (cond
	 ((null? l) 0)
	 (else 
	   (+ 1 (length (cdr l)))))))
   (lambda (x)
     ((mk-length mk-length) x)))))

(letrec
  ((even-p
     (lambda (x)
       (if (zero? x) #t (odd-p (- x 1)))))
   (odd-p
     (lambda (x)
       (if (zero? x) #f (even-p (- x 1))))))
  (cons (even-p 5) (odd-p 5)))

(define (fact n)
  (cond ((zero? n) 1)
	(#t (* n (fact (- n 1))))))

(a b c .())
