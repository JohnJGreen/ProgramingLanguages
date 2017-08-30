#lang racket

;John Green
;CSE 3302
;Lab 1

(define (bitcount num)
  (cond
    ;check if num is zero
    ((zero? num) 0)
    ;divide num by 2 and call bitcount on the quotient return the remainder
    (else (+ (bitcount (quotient num 2)) (modulo num 2)))))

(define (numCount lat num)
  (define (helper ls)
    (cond
      ;check for null list
      ((null? ls) 0)
      ;check for more then one element. if true compare first element and call numcount on the rest
      ((pair? ls) (+ (if (eqv? (car ls) num) 1 0) (numCount(cdr ls) num)))
      (else
       ;compare element and return 1 if the same
       (if (integer? ls)
           (if (= num ls) 1 0) 0))))
  (helper lat))

(define (numCountExp x num)
  (define (helper ls)
    (cond
      ;check for null list
      ((null? ls) 0)
      ;check for multiple elements if true call helper on first and rest elements
      ((pair? ls) (+ (helper (car ls)) (helper (cdr ls))))
      (else
       ;compare element and return 1 if the same
       (if (integer? ls)
           (if (= num ls) 1 0) 0))))
  (helper x))

(define (structurally? x y)
  (cond
    ;check if x,y are both lists
    ((and (eqv? (pair? x) #t) (eqv? (pair? y) #t))
        ;check if x,y are the same number of elements
        (if (= (length x) (length y))
            ;recursively check number of elements
            (and (structurally? (car x) (car y)) (structurally? (cdr x) (cdr y))) #f))
    ;check if x,y are both single elements
    ((and (eqv? (pair? x) #f) (eqv? (pair? y) #f)) #t)
    ;if you have a single element and a list
    (else #f)))
  

;(bitcount 0)
;(bitcount 1)
;(bitcount 10)
;(bitcount 65)
;(bitcount 32)
;(bitcount (- 32 1))
;(quote --------)
;(numCount '() 0)
;(numCount '(1 2 3 a 5 b 6 c "string" 7 5 9) 5)
;(numCount '(5 b 6 c "string" 7 5 1 2 3 a 9) 0)
;(numCount '(1 c "string" 7 5 2 3 a 5 b (6 (1 18 4) 3) 9) 1)
;(quote --------)
;(numCountExp '(1 2 (3 a 5) (b 6 c "string" 7 (5)) 9) 5)
;(numCountExp '(1 2 (3 a 5) (b 6 c ("string") 7 (5)) 9) 0)
;(numCountExp '(1 2 (3 a 5) 1 (b 6 () 1 c ("string") 7 (5)) 9) 1)
;(quote --------)
;(structurally? '(1 2 (3 a 5) (b 6 c "string" 7 (5)) 9) '(2 1 (3 "string" 5) (b 6 c a 7 (5)) 9))
;(structurally? '(1 2 (3 a b 5) (b 6 c "string" 7 (5)) 9) '(2 1 (3 "string" 5) (b 6 c d a 7 (5)) 9))
;(structurally? '(a b c d) '(b c d e f))
;(structurally? 'a 1234)
;(structurally? '(()) '())
