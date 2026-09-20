#lang typed/racket

(require typed/rackunit)

(struct Desk ([width : Real] [height : Real] [depth : Real])
  #:transparent)
(struct Bookshelf ([shelf-width : Real] [number-shelves : Real] [depth : Real])
  #:transparent)
(define-type OfficeFurniture (U Desk Bookshelf))

; computes the footprint of a bookshelf or desk
(define (furniture-footprint [furniture : OfficeFurniture]) : Real
  (match furniture
  [(Desk w h d) (* w d)]
  [(Bookshelf w s d) (* w d)]))

(check-equal? 25 (furniture-footprint (Desk 5 10 5)))
(check-equal? 72 (furniture-footprint (Bookshelf 8 5 9)))
(check-equal? 8 (furniture-footprint (Desk 2 5 4)))
#|
;(define (show-example b)
;          (begin
;             
; 
;(printf "sum of magnitude of elements: ~s\n"
;        (array-axis-fold (array-map magnitude (my-fft b)) 0 + 0))
; 
;                 (plot (points (in-array (array->plottable (my-fft b))))
;#:y-max 1500
;#:x-max 1024
;  #:height 300)))
;(printf "magnitude of sum of elements: ~s\n"
;                     (array-map magnitude (array-axis-fold (my-fft b) 0 + 0)))
;(define (srl:map-append func lst)
;  (cond [(null? lst) lst]
;      [else (append (func (car lst))
;              (srl:map-append func (cdr lst)))]))

|#

; takes in a list and returns a string consisting of the elements of the list in reverse order
(define (rev-str-app [input_list : (Listof String)]) : String
  (cond
    [(empty? input_list) ""]
    [else 
     (string-append (rev-str-app (rest input_list)) (first input_list))]))

(check-equal? (rev-str-app (list "o" "l" "l" "e" "h")) "hello")
(check-equal? (rev-str-app (list "st" "te")) "test")
(check-equal? (rev-str-app (list "1" "2" "3" "4" "5")) "54321")
(check-equal? (rev-str-app (list "ball" "juice" "frog")) "frogjuiceball")

#|
using (:print-type rev-str-app) returns (-> (Listof String) String). Basically the type is a function that takes in a list of strings and returns a string.
the type of + is a really long list of functions. It's really long because it needs to store all the different types of inputs and outputs to add and return.
|#