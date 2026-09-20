#lang typed/racket

(require typed/rackunit)

; This takes in an sexp and matches it to the form number symbol symbol or not

(define (parse000 [s : Sexp]) : Boolean
  (match s
    [(list r 'chris sym) (and(real? r) (symbol? sym)) #t]
    [other #f]
    ))

(check-equal? (parse000 '(1 chris a)) #t)
(check-equal? (parse000 '("one")) #f)

; parse 001
; this does the same thing as parse001 but has two return types
(define (parse001 [s : Sexp]) : (U Boolean Symbol)
  (match s
    [(list r 'chris sym)
     (cond [
            (and(real? r) (symbol? sym)) sym]
           [else #f])]
    [other #f]
    ))

(check-equal? (parse001 '(1 chris a)) 'a)
(check-equal? (parse001 '(1 chris 2)) #f)
(check-equal? (parse001 '("one")) #f)

; parse 002
; returns list on sexp lists of length 3 with the 2nd ele being a list
; returns false if none are matched


(define (parse002 [s : Sexp]) : (U Boolean (Listof Real))
  (match s
    [(list a l b)
         (cond
           [(and (list? l) (andmap real? l)) l]
           [else #f])]
     ; NOT in form a list b
     [other #f]
   ))


; note: somewhere up here ellipses could be used as well
(check-equal? (parse002 'chris) #f)
(check-equal? (parse002 '(1 (list 4 "k" 6) l)) #f)
(check-equal? (parse002 (list 1 (list 4 5 6) 'l)) (list 4 5 6))

; parse 003 : a list of a lists, where sublist must match len = 3
; returns false if not matched, returns the sum of all first eles - sum of all third eles

(define (parse003 [s : Sexp]) : (U Boolean Real)
  (match s
    [(list (list a b c) ...)
    ;; the ellipses matches many repetitions of that sub pattern
    ;; this didint work due to tr constraints
    ;;  [(list (list (? real? a) (? real? b) (? real? c)) ...)

     ; can a b c be mapped to reals?
     (if (and (andmap real? a) (andmap real? b) (andmap real? c))
         (- (apply + (cast a (Listof Real)))
            (apply + (cast c (Listof Real))))
         #f)]
    [other #f]))


(check-equal? (parse003 (list 'chris 'jerry)) #f)
(check-equal? (parse003 (list (list 4 5 6) (list 9 8 2 4))) #f)
(check-equal? (parse003 (list (list 4 5 6) (list 9 8 2))) 5)
(check-equal? (parse003 (list (list 4 2 4) (list 6 8 2))) 4)
(check-equal? (parse003 (list (list 4 5 6) (list 9 "hello" 2))) #f)