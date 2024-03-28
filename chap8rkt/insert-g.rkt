#lang racket

(define insert-g
  (lambda (seq)
    (lambda (new old l)
    (cond
      ((null? l) '())
      ((equal? (car l) old)
       (seq new old (cdr l))
	     (else (cons (car l)
			 ((insert-g test?) new old
					    (cdr l)))))))))

