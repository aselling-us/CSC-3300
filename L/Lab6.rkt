#lang racket
(require racket/match)
(require rackunit)
; Second: you can’t check equality of functions directly, which makes
;test cases difficult. When an exercise asks you to produce a
;function, the test case should go ahead and supply inputs to the
;function, so that you can test equality of numbers rather than
;functions.

((lambda (x) (+ x 2)) 3)

; out: 5

;((lambda (f g) (f (g 3)))
;  (lambda (x) (+ x 3))
;   (lambda (x) (* x 2)))
; this errors with type mismatch?

;; curried add

;Develop the curried-add function. It takes a number ’a’ and returns ;a function that takes a number ’b’ and returns a+b. In other words, ;it has the type

;    (number -> (number -> number))

;... where (t1 -> t2) is the type of a function that takes a t1 and ;produces a t2.


(define (curried-add a)
; return a function that takes in a number b
  (lambda (b) (+ a b)))

(check-equal? ((curried-add 5) 3) 8)
(check-equal? ((curried-add 5) 10) 15)

; Develop the curry2 function; it takes a function of
;two arguments ;f, and produces a function that we’ll call M.
;The function
;M takes ;one argument and produces a function that we’ll call N.
;N takes one ;argument and produces the result of calling the
;input ;function f on ;the two given arguments. In other words,
;it has the type

;    (All (a b c) ((a b -> c) -> (a -> (b -> c))))

;... for types a,b, and c. You will need lambda for this.

(define (curry2  f)
  (lambda (a)
    (lambda (b)
      (f a b)
    )
  ))

(check-equal? (((curry2 +) 5) 3) 8)

;Develop the curry3 function; it takes a function of three arguments,
;and produces a function that takes one argument and produces a
;function that takes one argument and produces a function that takes
;one argument and produces the result of calling the input function
;on the three given arguments. In other words, it has the type


   ; (All (a b c d) ((a b c -> d) -> (a -> (b -> (c -> d)))))

;... for types a,b,c, and d. You will need lambda for this.


(define (curry3  f)
  (lambda (a)
    (lambda (b)
      (lambda (c)
      (f a b c)
    )
  )))
(check-equal? ((((curry3 +) 5) 3) 1) 9)

; contains function to detect whether symbol appears in the list

(define (contains? lst sym)
  (cond
    [(null? lst) #f]
    ; car gets the first element of lst
    [(equal? (car lst) sym) #t]
    ; cdr gets the tail
    [else (contains? (cdr lst) sym)]))



(check-equal? (contains? '(apple banana cherry) 'dog) #f)
(check-equal? (contains? '() 'apple) #f)

(check-equal? (contains? '(apple banana cherry) 'cherry) #t)

;  consumes a source list of symbols and a list of query symbols, and
; returns a list of booleans indicating for the corresponding element
;of the query list whether it occurs in the source list.

(define (in-list-many? source queries) 
  (map ((curry2 contains?) source) queries))

(check-equal? (in-list-many? '(apple banana cherry) '(banana dog apple))
              '(#t #f #t))
 
