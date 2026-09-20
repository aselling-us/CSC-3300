#lang typed/racket


(require typed/rackunit)

;; Define a custom structure types for the bike types
(struct Trek ([n : Number]) #:transparent)
(struct Bianchi ([n : Number]) #:transparent)
(struct Gunnar ([n : Number]) #:transparent)


(define-type Bike (U Trek Bianchi Gunnar))

; a function that takes a bike list and scopes it to only treks
(define (only-treks [l : (Listof Bike)]): (Listof Trek)
                    (match l
                      ['() '()]
                      ; ADD HERE bike filtering to trek only
                      [(cons f r) (cond
                                    [(Trek? f) (cons f (only-treks r))]
                                       [else (only-treks r)]
                               )]
                    ))

; this function boils down l into only containing Bianchis
(define (only-bianchis [l : (Listof Bike)]): (Listof Bianchi)
                    (match l
                      ['() '()]
                      [(cons f r) (cond
                                    [(Bianchi? f) (cons f (only-bianchis r))]
                                       [else (only-bianchis r)]
                     )]
 ))


; this function takes in a type (Gunnar?) as a predicate and boils it down to not have ; Gunnar
(define (only-these [l : (Listof Bike)] [p : (-> Any Boolean)]) : (Listof Bike)

  (match l
                      ['() '()]
                      [(cons f r) (cond
                                    [(p f) (cons f (only-these r p ))]
                                       [else (only-these r p )]
                     )]
))


; function to combine all elements of an 
(define (my-append [l1 : (Listof Any)] [l2 : (Listof Any)]) : (Listof Any)

   (match l1
                      ['() l2]
                      [(cons f r) (cons f (my-append r l2)) ]
))

; given a list and int n, return the first n elements
(define (my-take [l : (Listof Any)] [n : Real] ) : (Listof Any)
  (cond
    [(<= n 0) '()] ; n is 0 or neg, or deducted to zero from prev call
    [else
     (match l
       ['() '()] ; empty list
       [(cons f r) (cons f (my-take r (+ -1 n)))])]))
;; tests
(check-equal? (only-treks '()) '())
(define b1 (Trek  3))
(define b2 (Bianchi 2))
(define b3 (Gunnar 1))
(check-equal? (only-treks (list b1 b2)) (list b1))
(check-equal? (only-bianchis (list b1 b2)) (list b2))

(check-equal? (only-these (list b1 b2 b3) Gunnar?) (list b3))

(check-equal? (my-append '() '()) '())
(check-equal? (my-append (list b1 b2) (list b2 b3)) (list b1 b2 b2 b3))
(check-equal? (my-append '() (list b2 b3)) (list b2 b3))
(check-equal? (my-append (list b2 b3) '()) (list b2 b3))

(check-equal? (my-take '() 1) '())
(check-equal? (my-take (list "a" "b" "c") 2) (list "a" "b"))
(check-equal? (my-take '(1 2 3 4 5) 2) '(1 2))

