#lang typed/racket

(require typed/rackunit)

;(struct NumC ([n : Real]) #:transparent)

(struct BinopC ([o : OpC] [l : ExprC] [r : ExprC]) #:transparent)

(define-type OpC (U '+ '- '* '/))

(struct PlusC ([l : ExprC] [r : ExprC]) #:transparent)

(struct MultC ([l : ExprC] [r : ExprC]) #:transparent)

(struct MinC ([l : ExprC] [r : ExprC]) #:transparent)

(struct DivC ([l : ExprC] [r : ExprC]) #:transparent)

(struct IfC ([c : ExprC] [t : ExprC] [f : ExprC]) #:transparent)

;(struct IdC ([s : Symbol]) #:transparent)

(struct AppC ([c : Symbol] [args : (Listof ExprC)]) #:transparent)

(define-type ExprC (U Real BinopC IfC AppC Symbol))

(struct FundefC([name : Symbol] [param : (Listof Symbol)] [body : ExprC]) #:transparent)

(struct ProgC ([fun : (Listof FundefC)]))

;parser for expressions in DMRG3, takes in the concrete syntax (a sexp from dmrg) and returns the abstract syntax

(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? s) s]
    [(? symbol? s) s]
    [(list '+ a b) (BinopC '+ (parse a) (parse b))]
    [(list '- a b) (BinopC '- (parse a) (parse b))]
    [(list '* a b) (BinopC '* (parse a) (parse b))]
    [(list '/ a b) (BinopC '/ (parse a) (parse b))]
    ;[(list (and (? symbol?) (or '+ '- '* '/)) a b) (BinopC (cast (first s) OpC) (parse a) (parse b))]
    [(list 'ifleq0? a b c) (IfC (parse a) (parse b) (parse c))]
    [(list (? symbol? s) a ...) (cond [(not (eq? s '->)) (AppC s (map parse a))]
                                      [else (error 'parse "DRMG invalid id, got ~e" s)])]))


;parses concrete syntax for function definitions and returns the abstract syntax

(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list (? symbol? s) (? symbol? p) ... '= b)
     (cond
       [(symbol-dup? (cast p (Listof Symbol)))
        (error 'parse-fundef "DRMG duplicate parameters, got ~e" p)]
       [else (FundefC s (cast p (Listof Symbol)) (parse b))])]
    [other (error 'parse-fundef "DRMG not a function, got ~e" s)]))

;helper function for parse-fundef and parse-prog and that checks if any symbols in a list share a name

(define (symbol-dup? [l : (Listof Symbol)]) : Boolean
  (match l
    ['() #f]
    [(cons f r) (or (and (not (empty? r)) (eq? f (first r))) (symbol-dup? r))]))


;parser for the program in DRMG3, this takes in the concrete syntax and returns the abstract syntax

(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    ['() '()]
    [(list f ...) (define result (cast (map parse-fundef f) (Listof FundefC)))
                  (cond
                    [(not (fun-dup? result)) result]
                    [else (error 'parse-prog "DRMG duplicate functions, got ~e" s)])]))

;helper function for parse-prog that checks if any functions share a name
;just converts the names to symbols and uses symbol-dup?

(define (fun-dup? [l : (Listof FundefC)]) : Boolean
  (symbol-dup? (map FundefC-name l)))


; finds and interprets the function main
(define (interp-fns [l : (Listof FundefC)]) : Real
  (match (find-fns l 'main)
    [(FundefC n p b) (interp b l)]))

;helper function for interp-fns

(define (find-fns [l : (Listof FundefC)] [s : Symbol]) : FundefC
  (match l
    ['() (error 'interp-fns "DRMG could not find main")];throw an error here
    [(cons (FundefC n p b) r) (cond
                                [(eq? n s) (FundefC n p b)]
                                [else (find-fns r s)])]))

; uses the list of funs to interpret all the expressions

(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(? symbol? s) (error 'interp "DRMG unknown symbol in expression")]
    [(? real? n) n]
    [(BinopC s l r) (binop s (interp l funs) (interp r funs))]
    [(IfC c t f) (cond [(<= (interp c funs) 0) (interp t funs)] [else (interp f funs)])]
    [(AppC f a) (match (find-fns funs f)
              [(FundefC _ p b) 
               (define argv (map (lambda ([arg : ExprC]) (interp arg funs)) a))
               (define new_body (subst p argv b))
               (interp new_body funs)])]))

; helper function of interp, matches symbol in binop and evaluates correct arithmetic

(define (binop [s : Symbol] [l : Real] [r : Real]) : Real
  (match s
    ['+ (+ l r)]
    ['- (- l r)]
    ['* (* l r)]
    ['/ (/ l r)]))


; helper for subst
; find 
(define (find-param [s : Symbol] [params : (Listof Symbol)] [args : (Listof Real)]) : Real
  (match (list params args)
    [(list (cons pf pr) (cons af ar))
     (if (eq? s pf) af (find-param s pr ar))]
    [_ (error 'find-param "symbol not found")]))
;substitues every occurence of a symbol in the first tree with the second tree



(define (subst [p : (Listof Symbol)] [r : (Listof Real)] [expr : ExprC]) : ExprC
  (match expr
    [(? symbol? s) (find-param s p r)]
    [(? real? n) n]
    [(BinopC s l r2) (BinopC s (subst p r l) (subst p r r2))]
    [(IfC c t f) (IfC (subst p r c) (subst p r t) (subst p r f))]
    [(AppC f a) (AppC f (map (lambda ([arg : ExprC]) (subst p r arg)) a))]
    ))


; how to recursive call on the list of arguments in ‘a. There is the same problem in interp, if we can figure that out everything should work



(check-equal? (parse 3) 3)
(check-equal? (parse 'a) 'a)
(check-equal? (parse '{+ a 7}) (BinopC '+ 'a 7))
(check-equal? (parse '{- 4 7}) (BinopC '- 4 7))
(check-equal? (parse '{* 4 7}) (BinopC '* 4 7))
(check-equal? (parse '{/ 4 7}) (BinopC '/ 4 7))
(check-equal? (parse '{ifleq0? 3 {+ 3 1} {- 3 1}})
              (IfC 3 (BinopC '+ 3 1) (BinopC '- 3 1)))
(check-equal? (parse '{five}) (AppC 'five '()))
(check-exn (regexp (regexp-quote "parse: DRMG invalid id, got '->"))
           (lambda () (parse '{-> 3 4})))


(check-equal? (parse-fundef '{add7 a = {+ a 7}}) (FundefC 'add7 (list 'a) (BinopC '+ 'a 7)))
(check-equal? (parse-fundef '{main = {add7 3}}) (FundefC 'main '() (AppC 'add7 (list 3))))
(check-equal? (parse-fundef '{pow a b = {ifleq0? b 1 {pow a {- b 1}}}})
              (FundefC 'pow (list 'a 'b) (IfC 'b 1
                                              (AppC 'pow (list 'a (BinopC '- 'b 1))))))
(check-exn (regexp (regexp-quote "parse-fundef: DRMG not a function, got '(+ 3 4)"))
           (lambda () (parse-fundef '{+ 3 4})))
(check-exn (regexp (regexp-quote "parse-fundef: DRMG duplicate parameters, got '(a a)"))
           (lambda () (parse-fundef '{add2 a a = {+ a a}})))


(check-equal? (parse-prog '{}) '())
(check-equal?
 (parse-prog '{{pow a b = {ifleq0? b 1 {pow a {- b 1}}}} {main = {pow 3 4}}})
 (list (FundefC 'pow (list 'a 'b) (IfC 'b 1
                                        (AppC 'pow (list 'a (BinopC '- 'b 1)))))
       (FundefC 'main '() (AppC 'pow (list 3 4)))))
(check-exn
 (regexp(regexp-quote "parse-prog: DRMG duplicate functions, got '((add2 a = a) (add2 c = c))"))
 (lambda () (parse-prog '{{add2 a = a} {add2 c = c}})))

;interp and subst are not finished yet

(check-equal? (interp-fns (list (FundefC 'main '() (BinopC '+ 3 4)))) 7)
(check-equal? (interp-fns (list (FundefC 'main '() (BinopC '- 3 4)))) -1)
(check-equal? (interp-fns (list (FundefC 'main '() (BinopC '* 3 4)))) 12)
(check-equal? (interp-fns (list (FundefC 'main '() (BinopC '/ 12 4)))) 3)
(check-equal? (interp-fns (list (FundefC 'main '() (BinopC '/ 12 4)))) 3)
(check-equal? (interp-fns (list (FundefC 'main '() (IfC 1 2 0)))) 0)
(check-equal? (interp-fns (list (FundefC 'main '() (IfC -1 2 0)))) 2)
(check-exn
 (regexp(regexp-quote "interp-fns: DRMG could not find main"))
 (lambda () (interp-fns (list (FundefC 'pow (list 'a 'b) (IfC 'b 1
                                                              (AppC 'pow (list 'a (BinopC '- 'b 1)))))
                              (FundefC 'add7 (list 'a) (BinopC '+ 'a 7))))))
(check-exn
 (regexp(regexp-quote "interp: DRMG unknown symbol in expression"))
 (lambda () (interp-fns (list (FundefC 'main '() (BinopC '+ 'a 3))))))

;(check-equal? (interp-fns (list (FundefC 'pow (list 'a 'b) (IfC 'b 1
;                                              (AppC 'pow (list 'a (BinopC '- 'b 1)))))
;                    (FundefC 'main '() (AppC 'pow (list 3 4))))) 0)

; cases for substr
; form list params list values exprc

(check-equal? (subst (list 'x) (list 5) (BinopC '+ 'x 3)) (BinopC '+ 5 3))
(check-equal? (subst (list 'x 'y) (list 5 2) (BinopC '* 'x 'y)) (BinopC '* 5 2))
; param synbol not exist
(check-exn (regexp (regexp-quote "find-param: symbol not found")) (lambda () (subst (list 'x 'y) (list 5 10) (BinopC '+ 'x 'z))))

; substr ifC
(check-equal? (subst (list 'x) (list 0) (IfC 'x 10 20)) (IfC 0 10 20))
; appc substr
(check-equal? (subst (list 'x 'y) (list 3 4) (AppC 'add (list 'x 'y))) (AppC 'add (list 3 4)))
; param not in exprc
(check-equal? (subst (list 'x) (list 5) (BinopC '+ 2 3)) (BinopC '+ 2 3))

(check-equal? (subst (list 'x) (list 5) 8) 8)

;f un c within func
;(check-equal? (interp (AppC 'add (list (BinopC '+ 1 2) 5))
;                      (list (FundefC 'add (list 'x 'y) (BinopC '+ 'x 'y))))
;             8)

(check-equal? (interp (AppC 'add (list 3 4))
                      (list (FundefC 'add (list 'x 'y) (BinopC '+ 'x 'y))))
              7)