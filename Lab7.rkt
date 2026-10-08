#lang racket

(require rackunit)

; param a number-like function n 
; returns function that applies f one more time than n does.
(define (add1 n)
  (lambda (f a)
    (f (n f a))))


; returns arg a
(define zero
  (lambda (f a) a))


; applies a to f
(define one
  (lambda (f a) (f a)))

; applies f to a twice
(define two
  (lambda (f a) (f (f a))))


;takes two funcs and returns a func of first applied to 2nd
; m applied to a g times, then m applied to that result f times
(define (add f g)
  (lambda (m a)
    (f m (g m a))
    )
  )

(define (racket-add1 n) (+ n 1))


(define (tru a b) a)


(define (fals a b) b)


(define (if f g h)
  ; if f = tru, return result of tru g, else return result fals h
  
  (f g h))




;Define a function called ’if’ that accepts three arguments. If the first one turns out to
;be the function tru (as above), it returns the result of the second argument. If the first
;one turns out to be the function fals (as above), it returns the result of the third
;argument.

(check-equal? (if tru "yes" "no") "yes")

(check-equal? (if fals "yes" "no") "no")


(check-equal? (zero racket-add1 5) 5)

(check-equal? (one racket-add1 5) 6)

(check-equal? (two racket-add1 5) 7)

(check-equal? ((add1 two) racket-add1 0) 3)

(check-equal? ((add one two) racket-add1 0) 3)

(check-equal? (tru 1 3) 1)
(check-equal? (fals 1 3) 3)




