#lang racket

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f) (le (lambda (x) ((f f) x)))))))

(define rember
  (lambda (x y)
    ((Y
       (lambda (f)
	 (lambda (l)
	   (cond
	     ((null? l) '())
	     ((equal? (car l) x) (cdr l))
	     (else (cons (car l) (f (cdr l)))))))) l)))

(define insertR
  (lambda (new old lat)
    ((Y
       (lambda (f)
	 (lambda (lat)
	   (cond)
	   ((null? l) '())
	   ((equal? (car lat) old)(cons old (cons new  (cdr lat))))
	   (else (cons (car lat) (f (cdr lat))))))) lat)))

(define subst2
  (lambda (new o1 o2 lat)
    ((Y
       (lambda (f)
	 (lambda (lat)
	   (cond
	     ((null? l) '())
	     ((or (equal? (car lat) o1) (equal? (car lat) o2))(cons new (cdr lat)))
	     (else (cons (car lat) (f (cdr lat)))))))) lat)))
