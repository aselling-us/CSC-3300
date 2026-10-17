#lang typed/racket

(require typed/rackunit)



; tbese are types the sexp (can) outputs after being parsed
(struct LeqC ([l : ExprC][r : ExprC])#:transparent)
(struct GrEqC ([l : ExprC][r : ExprC])#:transparent)
(struct bEqA ([l : ExprC][r : ExprC])#:transparent)


(struct AndC ([l : ExprC][r : ExprC])#:transparent) ; {and a b}
(struct OrC ([l : ExprC][r : ExprC])#:transparent) ; {or a b}
(struct NotC ([l : ExprC])#:transparent);{not a}

(struct IfC ([c : ExprC][t : ExprC][f : ExprC])#:transparent)

(struct ConsC ([h : ExprC] [t : ExprC])#:transparent)
(struct EmptyC () #:transparent)

(struct FirstC ([l : ExprC]) #:transparent)
(struct RestC ([l : ExprC]) #:transparent)

; this is a uniion of every struct we make 
(define-type ExprC (U Real Boolean Symbol AndC OrC NotC IfC ConsC ComparC EmptyC FirstC RestC)) 

(define-type Value (U Real Boolean (Listof Value)))

(define-type ComparC (U LeqC GrEqC bEqA))

(define-type BoolEvalC (U OrC AndC NotC))



;a sexp is internally defined as
; (define-type Sexp (U Real String Boolean .. Symbol (Listof Sexp)))
; quoting a dmrg expression gives a Sexp
; we leave the quote bc we are not trying to eval + 1 2

(check-equal? '{+ 1 2} '(+ 1 2))

(check-equal? '{<= 1 2} '(<= 1 2))

; tests of Sexp parsed into ExprC
; process for filling this in is taking what the sexp of each concrete syntax would look like and putting it into an ExprC, if that exprc does not already exist. ensuring that the arguments of ExprC like a, b , l r etc can be recursively evaluated
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) n]
    
    ; a quoted number is a number, parsing  that is still a number
    ['true #t]
    ['false #f]
    ['empty (EmptyC)]
    [(list '<= a b) (LeqC (parse a) (parse b))]
    [(list '== a b) (bEqA (parse a) (parse b))]
    [(list 'and a b) (AndC (parse a) (parse b))]
    [(list 'or a b) (OrC (parse a) (parse b))]
     [(list 'not a) (NotC (parse a))]
    [(list 'if c t f)
     (IfC (parse c) (parse t) (parse f))]
    [(list 'cons h t) (ConsC (parse h) (parse t))] ; maybe i need to parse h and t
    [(list 'first l) (FirstC (parse l))]
    [(list 'rest l) (RestC (parse l))]
    
  ))

;(define (parse-fundef [s : Sexp]) : FundefC ...)
;(define (parse-prog [s : Sexp]) : (Listof FundefC) ...)

(check-equal? (parse '(<= 1 2)) (LeqC 1 2))
(check-equal? (parse '(== 1 2)) (bEqA 1 2))
(check-equal? (parse '(and true true)) (AndC #t #t))
(check-equal? (parse '(and true false)) (AndC #t #f))
(check-equal? (parse '(or true false)) (OrC #t #f))
(check-equal? (parse '(not true)) (NotC #t))
(check-equal? (parse '(if true true true)) (IfC #t #t #t))
(check-equal? (parse '(if false 3 5)) (IfC #f 3 5))
; symbol empty should return emptyC which will be parsed to empty list
(check-equal? (parse '(cons 1 empty)) (ConsC 1 (EmptyC)) )
(check-equal? (parse '(first (list 1 2 3))) (FirstC (parse '(list 1 2 3))))
(check-equal? (parse '(first (list 1 2 3))) (FirstC (ConsC 1 (ConsC 2 (ConsC 3 (EmptyC))))))
(check-equal? (parse '(rest (list 1 2 3))) (RestC (parse '(list 1 2 3))))

;; for interp
; [(== 'empty) '()]