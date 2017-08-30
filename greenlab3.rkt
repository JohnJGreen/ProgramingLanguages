#lang racket

;John Green
;CSE 3302
;Lab 3


(define (list2bag list)
  (define (bag ls)
    (cons (minimum ls) (cons (numcount (minimum ls) ls) '())))
  (define (helper ls)
    (cond
      ((null? ls) ls)
      (else (cons (bag ls) (helper (delete (minimum ls) ls))))))
  (helper list))

(define (numcount item list)
  (define (helper ls)
    (cond
      ((null? ls) 0)
      ((pair? ls) (+ (if (eqv? (car ls) item) 1 0) (helper (cdr ls))))))
  (helper list))

(define (delete item list)
  (define (helper ls)
    (cond
     ((null? ls) ls)
     ((equal? item (car ls)) (helper (cdr ls)))
     (else (cons (car ls) (helper (cdr ls))))))
  (helper list))

(define (minimum ls)
  (cond
    ((null? (cdr ls)) (car ls))
    ((< (car ls) (minimum (cdr ls))) (car ls))
    (else (minimum (cdr ls)))))

               
'list2bag
(list2bag '(5))
(list2bag '())
(list2bag '(5 5))
(list2bag '(1 4 3 2 3 2 1 5 1 5 4))