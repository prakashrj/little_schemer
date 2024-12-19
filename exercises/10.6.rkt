#lang racket

(lambda (entry table)
  (lambda (name)
    (cond
      ((member? name (first entry))
       (pick (index name (first entry))
	     (second entry)))
      (else (table name)))))
