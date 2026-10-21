#lang typed/racket

; step 4.3
(require typed/rackunit)
(require (only-in lang/htdp-beginner string-ith))
 
(+ 3 4)
(+ 4 5)
(check-equal? (* 4 13) 52)
true
false
(or false true)
(check-equal? (or false true) #t)

; add two numbers together
(define (add-nums [a : Integer] [b : Integer]) : Integer
  (+ a b))
(add-nums 8 9)
(check-equal? (add-nums 8 9) 17)

; Exercise 15. Define ==>. The function consumes two Boolean values,
; call them sunny and friday. Its answer is #true if sunny is false or
; friday is true. Note Logicians call this Boolean operation implication,
; and they use the notation sunny => friday for this purpose.
(define(==> [sunny : Boolean] [friday : Boolean]): Boolean
  (or (not sunny)friday))
(check-equal? (==> #f #t) #t)
(check-equal?(==> #f #f) #t)
(check-equal?(==> #t #t) #t)
(check-equal?(==> #t #f) #f)

; Exercise 19. Define the function string-insert, which consumes a
; string str plus a number i and inserts "_" at the ith position of str.
; Assume i is a number between 0 and the length of the given string (inclusive).
; See exercise 3 for ideas. Ponder how string-insert copes with "".
(define(string-insert [str : String][i : Integer]): String
 (string-append (substring str 0 i) "_" (substring str i (string-length str))))
(check-equal?(string-insert  "hello" 0) "_hello")
(check-equal?(string-insert  "hello" 4) "hell_o")
(check-equal?(string-insert  "hello" 2) "he_llo")

; problem
;The owner of a monopolistic movie theater in a small town has complete freedom
; in setting ticket prices. The more he charges, the fewer people can afford tickets.
; The less he charges, the more it costs to run a show because attendance goes up. In
; a recent experiment the owner determined a relationship between the price of a ticket
; and average attendance.

;At a price of $5.00 per ticket, 120 people attend a performance. For each 10-cent change
;in the ticket price, the average attendance changes by 15 people. That is, if the owner
;charges $5.10, some 105 people attend on the average; if the price goes down to $4.90,
;average attendance increases to 135. Let’s translate this idea into a mathematical formula

; ==== CONSTANTS =========== 

(: BASE-PRICE Real)
(: ATTENDANCE-CHANGE Real)
(: PRICE-CHANGE Real)
(: FIXED-COST Real)
(: VARIABLE-COST-PER-ATTENDEE Real)

(define BASE-ATTENDANCE : Real 120)
(define BASE-PRICE 5.0)
(define ATTENDANCE-CHANGE 15)
(define PRICE-CHANGE 0.1)
(define FIXED-COST 180)
(define VARIABLE-COST-PER-ATTENDEE 0.04)

; ==== Definitions
(define (attendees [ticket-price : Real]) : Real
  (- BASE-ATTENDANCE (* (- ticket-price BASE-PRICE) (/ ATTENDANCE-CHANGE PRICE-CHANGE))))

(define (revenue [ticket-price : Real]) : Real
  (* ticket-price (attendees ticket-price)))

(define (cost [ticket-price : Real]) : Real
  (+ FIXED-COST (* VARIABLE-COST-PER-ATTENDEE (attendees ticket-price))))

(define (profit [ticket-price : Real]) : Real
  (- (revenue ticket-price)
     (cost ticket-price)))

(check-equal? (attendees 5.00) 120.0)
(check-= (attendees 5.10) 105.0 .001)
(check-= (attendees 4.90) 135.0 .001)
(check-equal? (revenue 5.00) 600.0)
(check-equal? (cost 5.00) 184.8)

; === 4.2========
; Develop the function interest. Like interest-rate, it consumes a deposit amount.
; Instead of the rate, it produces the actual amount of interest that the money earns
; in a year. The bank pays a flat 4% for deposits of up to $1,000, a flat 4.5% per year
; for deposits of up to $5,000, and a flat 5% for deposits of more than $5,000.
(define (interest-rate [deposit : Real]) : Real
  (cond
    ; if statement
    [(<= deposit 1000) 0.04]
    ; if statement
    [(<= deposit 5000) 0.045]
    [else 0.05]))

(define (interest [deposit : Real]) : Real
  (* deposit (interest-rate deposit)))


(check-= (interest-rate 1000) .04 .001)
(check-= (interest-rate 1010) .045 .001)
(check-= (interest-rate 5011) .05 .001)
(check-= (interest 1000) 40.0 .001)
(check-= (interest 1010) 45.45 .00001)
(check-= (interest 5011) 250.55 .001)