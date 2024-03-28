#lang racket

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
		  (firsts (cdr l)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
	((member (car lat) (cdr lat)) #f)
	(else (set? (cdr lat))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define rel '((d 4) (b 0) (b 9) (e 5) (g 4)))

(fun? rel)


