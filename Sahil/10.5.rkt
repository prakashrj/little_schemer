#lang racket

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      (else (and (atom? (car l)) (lat? (cdr l)))))))

(define member?
(lambda (a lat)
(cond
((null? lat) #f)
(else (or (eq? (car lat) a)
(member? a (cdr lat))))))) 

(define table?
  (lambda (l)
    (cond
      ((null? l) #t)
      (else (and (entry? (car l)) (lat? (cdr l)))))))

(define set?
  (lambda (l)
    (cond
      ((null? l) #t)
      (else (and (atom? (car l)) (set? (cdr l)) (not (member? (car l) (cdr l))))))))

(define entry?
  (lambda (l)
    (cond
      ((and (eq? (length l) 2) (set? (car l)) (eq? (length (car l)) (length (second l))))))))

(define a-pair?
  (lambda (l)
    (cond
      ((null? l) #f)
      ((atom? l) #f)
      ((null? (cdr l)) #f)
      ((null? (cdr (cdr l))) #t))))

(define first
  (lambda (pair)
    (car pair)))

(define second
  (lambda (pair)
    (car (cdr pair))))

(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2 '()))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
        (+ (length* (first pora))
           (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
        (+ (* (weight* (first pora)))
           (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

(define new-entry
  build)

(define look-up-in-entry
  (lambda (name entry entry-f)
    (look-up-in-entry-helper name (first entry) (second entry)
                             entry-f)))

(define look-up-in-entry-helper
  (lambda (name names vals entry-f)
    (cond
      ((null? names)
       (entry-f name))
      ((eq? (car names) name)
       (car vals))
      (else (look-up-in-entry
              name
              (build (cdr names)
                     (cdr vals))
              entry-f)))))

(define extend-table
  cons)

(define look-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (look-up-in-entry name (car table)
                              (lambda (name)
                                (look-in-table name (cdr table)
                                               table-f)))))))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((or (number? e)
           (eq? e #t)
           (eq? e #f)
           (eq? e 'cons)
           (eq? e 'car)
           (eq? e 'cdr)
           (eq? e 'null?)
           (eq? e 'eq?)
           (eq? e 'atom?)
           (eq? e 'zero?)
           (eq? e 'add1)
           (eq? e 'sub1)
           (eq? e 'number?))
       *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((equal? (car e) 'lambda) *lambda)
      ((equal? (car e) 'quote) *quote)
      ((equal? (car e) 'cond) *cond)
      (else *application))))

(define *const
  (lambda (e table)
    e))

(define *identifier
  (lambda (e table)
    (look-in-table e table initial-table)))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *lambda
  (lambda (e table)
      (cons table (cdr e))))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))
(define else?
  (lambda (question)
    (cond
      ((atom? question) (eq? question 'else))
      (else #f))))
(define question-of first)
(define answer-of second)

(define *application
  (lambda (e table)
    (apply
      (meaning (car e) table)
      (evlis (cdr e) table))))

(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else
        (cons (meaning (car args) table)
              (evlis (cdr args) table))))))

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive
         fun vals))
      ((non-primitive? fun)
       (apply-closure
         fun vals)))))

(define primitive?
  (lambda (e)
    (or (number? e)
           (eq? e #t)
           (eq? e #f)
           (eq? e 'cons)
           (eq? e 'car)
           (eq? e 'cdr)
           (eq? e 'null?)
           (eq? e 'eq?)
           (eq? e 'atom?)
           (eq? e 'zero?)
           (eq? e 'add1)
           (eq? e 'sub1)
           (eq? e 'number?))))

(define non-primitive?
  (lambda (l)
   (cond
    ((eq? (length l) 3) 
      (and (table? (car l)) (lat? (second l)) (pair? l))))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'car)
       (car (first vals)))
      ((eq? name 'cdr)
       (cdr (first vals)))
      ((eq? name 'cons)
       (cons (first vals) (second vals)))
      ((eq? name 'add1)
       (add1 (first vals)))
      ((eq? name 'sub1)
       (sub1 (first vals)))
      ((eq? name 'zero?)
       (zero? (first vals)))
      ((eq? name 'number?)
       (number? (first vals)))
      ((eq? name 'eq?)
       (eq? (first vals) (second vals)))
      ((eq? name 'null?)
       (null? (first vals)))
      ((eq? name 'atom?)
       (:atom? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((primitive? x) #t)
      ((non-primitive? x) #t)
      (else #f))))

; Closures take the form of
; (table formal-args body)
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
       (extend-table
        (new-entry
          (formals-of closure)
          vals)
        (table-of closure)))))
(define table-of first)
(define formals-of second)
(define body-of third)

(define initial-table
  (lambda (name)
    (error
      (format "Table lookup for ~a failed" name))))

(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ 1 (length (cdr l)))))))

(define e3 '((lambda (x) ((lambda (x) (add1 x)) (add1 4))) 6))
(length '(a b c))
(meaning '6 '())
(evlis '(6) '())
(meaning '(lambda (x) ((lambda (x) (add1 x)) (add1 4))) '())
(value e3)
(set? '(a b b))
(non-primitive? '(() (x) ((lambda (x) (add1 x)) (add1 4))))
