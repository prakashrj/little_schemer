#lang racket

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
    (cond
      ((null? l) '())
      ((test? (car l) old)
       (cons new (cons old (cdr l))
	     (else (cons (car l)
			 ((insertL-f test?) new old
					    (cdr l))))))))))
