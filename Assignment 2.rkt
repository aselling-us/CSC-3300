#lang typed/racket

(require typed/rackunit)
(require racket/math)
;  Exercise 2.3.3.   An old-style movie theater has a simple profit function. Each customer pays $5 per ticket.
; Every performance costs the theater $20, plus $.50 per attendee. Develop the function total-profit.
; It consumes the number of attendees (of a show) and produces how much income the attendees produce.

; The function movie-profit takes in the number of attendees
;for a showing and produces profit after income and expenses.

(define (total-profit [n : Real]) : Real
  (+ -20 (* .50 n) (* n 5))) 
 

(check-equal? (total-profit 0) -20)
(check-equal? (total-profit 50) 255.0) 

;  Exercise 3.3.3.   Develop area-cylinder. The program consumes the radius of
;the cylinder's base disk and its height.
;Its result is the surface area of the cylinder.    Solution

; The function area-cylinder takes in two parameters
;(radius and height) and
;returns the surface area of a cylinder. follows formula
; A = 2 pi r h + 2 pi r r

(define (area-cylinder [r : Real] [h : Real])
  (+ (* 2 pi r h)(* 2 pi r r))
  )

(check-equal? (area-cylinder 0 0) 0)
(check-= (area-cylinder 2 5) 87.96459 .01)
(check-= (area-cylinder 6 8) 527.78757 .01)

;; 2.2
; ; a magic-trick is either
; - (Card-Trick decks volunteers),
;    where decks is a number and volunteers is an integer,
;    representing a card trick that requires the given
;    number of card decks and audience volunteers, or
; - (Guillotine realism has-tiger?),
;    where realism is a number indicating the level of
;    realism from 0 up to 10 and has-tiger? is a boolean,
;    representing a guillotine trick with the given level
;    of realism and an optional tiger.


(struct Card-Trick ([decks : Number]  [volunteers : Number])#:transparent)
(struct Guillotine ([realism : Number]  [has-tiger? : Boolean])#:transparent)
;realism 0-10, has-tiger optional
(define-type magic-trick (U Card-Trick Guillotine))

;Develop the function trick-minutes, that computes how long a trick will take.
;Assume that a ;card trick requires one minute per deck of cards and that its
;length is doubled for every ;volunteer required, and that a guillotine trick
;requires ten minutes unless it has a tiger, ;in which case it requires 20.

(define (trick-minutes [t : magic-trick]) : Number
  ; determine whether Card or Guillotine
  (match t
    [(Guillotine r has-tiger?)
     (if has-tiger? 20 10)
     ]
    [(Card-Trick d v)
     (* d (expt 2 v))
     ]
    )
  )

(check-equal? (trick-minutes (Guillotine 8 #f)) 10)
(check-equal? (trick-minutes (Guillotine 6 #t)) 20)

(check-equal? (trick-minutes (Card-Trick 1 0)) 1) 
(check-equal? (trick-minutes (Card-Trick 3 0)) 3)   
(check-equal? (trick-minutes (Card-Trick 1 1)) 2)
(check-equal? (trick-minutes (Card-Trick 2 2)) 8)

;; 2.3, 2.4

; Develop a data definition for a polynomial (with type name Polynomial) that
;includes variants for linear (Ax + B) and quadratic (Ax^2 + Bx + C)
;polynomials of one variable. Call the variants Linear and Quadratic; each
;should accept the coefficients in the order given here (i.e., A comes first).
;For the purposes of this and the next problem, you should assume that the
;first coefficient (A) of a quadratic polynomial is never zero. The first
;coefficient of a linear polynomial, however, may be zero.

(struct Linear ([a : Number] [b : Number])#:transparent)
(struct Quadratic ([a : Number] [b : Number] [c : Number])#:transparent)
;;assume a !=0

(define-type Polynomial (U Linear Quadratic))

(define (interp [p : Polynomial] [x :  Number]): Number
  (match p
    [(Linear a b)
     (+ (* a x) b)
     ]
    [(Quadratic a b c)
     (+ (* a x x) (* b x) c)
     ]
    )
  )

(check-equal? (interp (Linear 0 0) 0)0)

(check-equal? (interp (Linear 3 4) 2)10)
(check-equal? (interp (Quadratic 3 4 5) 2)25)

; this function takes in a linear or quadratic polynomial and returns its
;derivative, which can be either a constant or a linear polynomial
(define (derivative [p : Polynomial ]) : (U Number Linear)
  (match p
    [(Linear a b)
     a
     ]
    [(Quadratic a b c)
     (Linear (* 2 a) b)
     ]
    )
  )

(check-equal? (derivative (Linear 0 0)) 0 )
(check-equal? (derivative (Linear 2 3)) 2 )
(check-equal? (derivative (Quadratic 2 3 4)) (Linear 4 3))
(check-equal? (derivative (Quadratic 10 0 3)) (Linear 20 0))

;; 2.5
;; type: btree no values at interior nodes, symbols possibly at leaves. Leaves
;can be either SymLeaf or BareLeaf. Node struct needed too

(struct Node ([left : BTree] [right : BTree]) #:transparent)

(struct SymLeaf ([s : Symbol])#:transparent)
(struct BareLeaf []#:transparent)
(define-type BTree (U Node SymLeaf BareLeaf))


(define example-1 : BTree (BareLeaf))
(define example-2 : BTree (Node (SymLeaf 'apple)(BareLeaf)))
(define example-3 : BTree (Node
                           (Node (BareLeaf)(BareLeaf))
                           (Node (BareLeaf)(BareLeaf))))

;; 2.6

; this function takes in a btree and produces a new btree that is the
;mirroed version

(define (mirror [b : BTree]) : BTree
  (match b
    [(BareLeaf) b]
    [(SymLeaf s) b] 
    [(Node l r) (Node (mirror r) (mirror l))]
    ))
(check-equal? (mirror (BareLeaf)) (BareLeaf))
(check-equal? (mirror (Node (BareLeaf)(BareLeaf))) (Node (BareLeaf)(BareLeaf)))
(check-equal? (mirror (Node (SymLeaf 'apple)(SymLeaf 'pear))) (Node (SymLeaf
                                                                  'pear)(SymLeaf 'apple)))

;; 2.7

; accepts in a binary  tree and produces the length of the shortest path to
;sym-leaf
(define (min-depth [b : BTree]) : (U Real String)
  (match b
    [(BareLeaf) "bonk"]
    [(SymLeaf s) 0]
    [(Node l r)
     (let ([dl (min-depth l)] [dr (min-depth r)])
       (if (string? dl)
           (if (string? dr)
               "bonk"           
               (+ 1 dr) )             
           (if (string? dr)
               (+ 1 dl)              
               (+ 1 (min dl dr)))) 
       )]))

    
(check-equal? (min-depth (SymLeaf 'apple)) 0)
(check-equal? (min-depth (BareLeaf)) "bonk")
(check-equal? (min-depth (Node (BareLeaf)(BareLeaf))) "bonk")

(check-equal? (min-depth (Node (SymLeaf 'apple)(BareLeaf))) 1)

(check-equal? (min-depth (Node (Node (BareLeaf) (SymLeaf 'hello))(SymLeaf 'orange))) 1)

; 2.8
; Double Containment
; contains-twice: given a binary tree and a symbol, return true if the tree
;contains two leaves w the given symbvol

;helper: count-symbol
; purpose: count how many leaves match symbol s in a given tree
(define (count-symbol [b : BTree] [s : Symbol]): Real
  (match b
    [(BareLeaf) 0]
    [(SymLeaf sl) (if (eq? sl s) 1 0)]
    [(Node l r) (+ (count-symbol l s) (count-symbol r s))]
    ))

(define (contains-twice? [b : BTree] [s : Symbol]) : Boolean
  (if (>= (count-symbol b s) 2) #t #f))

  (check-equal? (count-symbol (BareLeaf) 'apple) 0)
  (check-equal? (count-symbol (SymLeaf 'apple) 'apple) 1)
(check-equal? (count-symbol (SymLeaf 'apple) 'orange) 0)
    (check-equal? (count-symbol (Node (SymLeaf 'apple)(SymLeaf 'apple)) 'apple) 2)

  (check-equal? (contains-twice? (BareLeaf) 'apple) #f)
  (check-equal? (contains-twice? (Node (SymLeaf 'apple)(SymLeaf 'apple)) 'apple) #t)

; 2.9
; sing the same data definition, develop the subst function that accepts a
;source BTree and a symbol and a replacement BTree and returns a new tree where
;very SymLeaf of the source tree containing the symbol is replaced by the
;replacement tree. (Make sure the source tree is the first argument to the
;subst function, or my tests will all fail....)
; subst: function. given a BTree and a Symbol and a Replacement Btree, return a
;new tree with all symbolsmatching s are replaced by the replacement tree
(define (subst [b_orig : BTree] [s : Symbol] [b_repl : BTree]): BTree
    (match b_orig
      [(BareLeaf) b_orig]
      [(SymLeaf sl) (if (eq? sl s) b_repl b_orig) ]
      [(Node l r) (Node (subst l s b_repl) (subst r s b_repl))]
    ))
(check-equal? (subst (BareLeaf) 'apple (BareLeaf)) (BareLeaf))
(check-equal? (subst (Node (SymLeaf 'pear) (BareLeaf)) 'apple (BareLeaf)) (Node (SymLeaf 'pear) (BareLeaf)) )
(check-equal? (subst (Node (SymLeaf 'pear) (BareLeaf)) 'pear (BareLeaf)) (Node (BareLeaf) (BareLeaf)) )

(check-equal? (subst
               (Node
                (Node (SymLeaf 'pear) (BareLeaf))
                (Node (SymLeaf 'pineapple) (SymLeaf 'strawberry)))

               'strawberry (BareLeaf)) (Node
                (Node (SymLeaf 'pear) (BareLeaf))
                (Node (SymLeaf 'pineapple) (BareLeaf))) )
                          
                          