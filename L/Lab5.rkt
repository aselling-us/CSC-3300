#lang typed/racket

(require typed/rackunit)

; This is the concrete syntax
;   Arith = num
;   |	{+ Arith Arith}
;   |	{* Arith Arith}

; We will eventually make a parser that takes this abstract syntax and converts
;concrete into abstract
(struct plusC ([l : ArithC] [r : ArithC]) #:transparent)
(struct multC ([l : ArithC] [r : ArithC]) #:transparent)
(struct numC ([n : Number]) #:transparent)
(struct powC ([n : ArithC]) #:transparent)

(define-type ArithC (U plusC numC multC powC))

(define (swap-adds [a : ArithC]) : ArithC
  (fprintf (current-output-port) (string-append (~a a) "\n"))
  (match a
    [(plusC l r) (plusC (swap-adds r)(swap-adds l))]
    [(multC l r) (multC (swap-adds l)(swap-adds r))] ; no change
    [(numC n) (numC n)])) ; no change




(check-equal? (swap-adds (numC 3)) (numC 3))

(check-equal? (swap-adds (multC (numC 3) (numC 9)))  (multC (numC 3) (numC 9)))

(check-equal? (swap-adds (plusC (numC 3) (numC 9))) (plusC (numC 9) (numC 3)))

;transforms concrete to the abstract (racket)
(define (parser [s : Sexp]) : ArithC
  [match s
    [(list '+ l r) (plusC (parser l) (parser r))]
    [(list '* l r) (multC (parser l) (parser r))]
    [(list ^2 x) (powC (parser x))]
    [(? number? n) (numC n)]
   ]
)

(check-equal? (parser '{+ 3 9}) (plusC (numC 3) (numC 9)))

(check-equal? (parser '{* 3 9}) (multC (numC 3) (numC 9)))

(check-equal? (parser '{^2 3}) (powC (numC 3))) 

; takes arith c and returns the racket syntax
(define (interp [a : ArithC]) : Number
  (match a
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]
    [(numC n) n]
    [(powC n) (* (interp n) (interp n))]
 
  ))

(check-equal? (interp (numC 3))3)

(check-equal? (interp (multC (numC 3) (numC 9))) 27)

(check-equal? (interp (plusC (numC 3) (numC 9))) 12)

(check-equal? (interp (powC (numC 3) )) 9)

; Develop the one-line function top-interp, that accepts an s-expression and calls the parser and then the interp function.

(define (top-interp [s : Sexp]) : Number
  (interp (parser s))
)

(check-equal? (top-interp '{^2 3}) 9)
(check-equal? (top-interp '{* 3 9}) 27)
(check-equal? (top-interp '{+ 3 9}) 12)

;Develop the zip function, that consumes two lists of Number of the same length ;and returns a new list of lists where each element of the new list is a list ;containing both of the corresponding elements from the original lists. Read that ;last sentence carefully.

(define (zip [l1 : (Listof Number)] [l2 : (Listof Number)]) : (Listof (Listof Number))
  (cond
    [(or (empty? l1) (empty? l2 )) '()]
    [else (cons (list (first l1) (first l2)) (zip (rest l1) (rest l2) ))]
    
  ; how do we hececk
    )
  )

(check-equal? (zip '() '()) '())
(check-equal? (zip (list 3 2 1) (list 9 2 3)) (list (list 3 9) (list 2 2) (list 1 3)))