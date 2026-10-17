#lang typed/racket

(require typed/rackunit)

;Full project implemented

;(struct NumC ([n : Real]) #:transparent)

(struct BinopC ([o : OpC] [l : ExprC] [r : ExprC]) #:transparent)

(define-type OpC (U '+ '- '* '/))

(struct PlusC ([l : ExprC] [r : ExprC]) #:transparent)

(struct MultC ([l : ExprC] [r : ExprC]) #:transparent)

(struct MinC ([l : ExprC] [r : ExprC]) #:transparent)

(struct DivC ([l : ExprC] [r : ExprC]) #:transparent)

(struct IfC ([c : ExprC] [t : ExprC] [f : ExprC]) #:transparent)

;(struct IdC ([s : Symbol]) #:transparent)

(struct AppC ([c : ExprC] [args : (Listof ExprC)]) #:transparent)

(struct StrV ([s : String]) #:transparent)


(struct StrC ([s : String]) #:transparent)

(define-type ExprC (U Real Boolean IfC AppC Symbol LocalC LambdaC FundefC BinopC StrC))

(struct FundefC([name : Symbol] [param : (Listof Symbol)] [body : ExprC]) #:transparent)

(struct ProgC ([fun : (Listof FundefC)]))

(struct LambdaC ([p : (Listof Symbol)] [b : ExprC]) #:transparent)

(struct LocalC ([p : (Listof Symbol)] [arg : (Listof ExprC)] [b : ExprC]) #:transparent)

(struct Env ([l : (Listof Binding)]) #:transparent)

(struct Binding ([p : Symbol] [e : Value]) #:transparent)

;values
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)



(struct CloV ([p : (Listof Symbol)] [b : ExprC] [clo-env : Env]) #:transparent)
(struct PrimV ([op : (U '+ '- '* '/ '<= 'substring 'strlen 'equal? 'true 'false 'error)]))
(define-type Value (U NumV BoolV CloV PrimV StrV
                      ))


(define top-env (Env (list (Binding '+ (PrimV '+)) (Binding '- (PrimV '-))
                           (Binding '* (PrimV '*)) (Binding '/ (PrimV '/))
                           (Binding '<= (PrimV '<=))
                           (Binding 'equal? (PrimV 'equal?))
                           (Binding 'true (PrimV 'true))
                           (Binding 'false (PrimV 'false))
                           (Binding 'substring (PrimV 'substring))
                           (Binding 'strlen (PrimV 'strlen))
                           )))
;parser for expressions in DMRG3,
;takes in the concrete syntax (a sexp from dmrg) and returns the abstract syntax

(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? s) s]
    [(? boolean? s) s]
    [(? string? s) (StrC s)]
    [(list '% p ... '-> b) (cond
                             [(symbol-dup? (cast p (Listof Symbol)))
                              (error 'parse "DRMG duplicate lambda parameters, got ~e" p)]
                             [else
                              (LambdaC (cast p (Listof Symbol)) (parse b))])]
    [(list 'local (list x1 '= e1) ... 'in eb) (AppC (LambdaC (append (cast x1 (Listof Symbol)))
                                                             (parse eb))
                                                    (map (lambda ([arg : Sexp]) (parse arg))
                                                         (cast e1 (Listof Sexp))))]
    [(? symbol? s) (cond [(or
                           ;(equal? s '+) (equal? s '-) (equal? s '*) (equal? s '/)
                           (equal? s 'if) (equal? s '->) (equal? s '=)
                           (equal? s '%) (equal? s 'local))
                          (error 'parse "DRMG invalid symbol, got ~e" s)]
                         [else s])]
    ;[(list '+ a b) (BinopC '+ (parse a) (parse b))]
    ;[(list '- a b) (BinopC '- (parse a) (parse b))]
    ;[(list '* a b) (BinopC '* (parse a) (parse b))]
    ;[(list '/ a b) (BinopC '/ (parse a) (parse b))]
    ;[(list (and (? symbol?) (or '+ '- '* '/)) a b) (BinopC (cast (first s) OpC) (parse a) (parse b))]
    [(list 'if a b c) (IfC (parse a) (parse b) (parse c))]
    [(list s a ...) (AppC (parse s) (map parse a))]
    
    ))





; uses the list of funs to interpret all the expressions

(define (interp [exp : ExprC] [env : Env]) : Value
  (match exp
    [(? symbol? s) (find-param-env s env)]
    [(? real? n) (NumV n)]
    [(? boolean? b) (BoolV b)]
    ;[(BinopC s l r) (binop s (interp l funs env) (interp r funs env))]
    [(IfC c t f) (match (interp c env)
                   [(BoolV #t) (interp t env)]
                   [(BoolV #f) (interp f env)])]
    [(LambdaC p b) (CloV p b env)]
    [(AppC f a)
     (define funval (interp f env))
     (define argv (map (lambda ([arg : ExprC]) (interp arg env)) a))
     (match funval
    [(CloV p b clo-env)
        (cond [(not (equal? (length p) (length argv)))
            (error 'interp "DRMG inequal parameters and arguments")]
           [else
        (define new-env (extend-env clo-env p argv))
        (interp b new-env)])]
     [(PrimV v)
        (prim-eval v argv)])]

     [(StrC s) (StrV s)]
    
    ))

; top interp of the program, converts concrete syntax to a result

(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

;helper functions

;helper function for parse-fundef and parse-prog that checks if any symbols in a list share a name

(define (symbol-dup? [l : (Listof Symbol)]) : Boolean
  (match l
    ['() #f]
    [(cons f r) (or (and (not (empty? r)) (eq? f (first r))) (symbol-dup? r))]))

;adds new bindings to the environment

(define (extend-env [env : Env] [p : (Listof Symbol)] [r : (Listof Value)]) : Env
  (match env
    [(Env b) (Env (append (map (lambda(a b) (Binding a b)) p r) b))]))

; helper function of interp, matches symbol in binop and evaluates correct arithmetic

(define (prim-eval [v : Symbol] [l : (Listof Value)]) : Value
  (match l
    ; matches to prim real a math operation
    [(list (NumV ln) (NumV rn)) (prim-real v ln rn)]
    ; matches on strlen string 
    [(list (StrV s)) (if (equal? v 'strlen)
                         (prim-strlen s)
                         (error v "DRMG primitive ~a got wrong arguments, got ~e" v l))]
    [(list (StrV s) (NumV start) (NumV stop)) (prim-substring s start stop)]
    ['() (prim-bool-lit v)]
     [_ (error v "DRMG primitive ~a got wrong arguments, got ~e" v l)]
    ))
(define (prim-bool-lit [s : Symbol]) : Value
  (match s
    ['true (BoolV #t)]
    ['false (BoolV #f)]
    )
  )

(define (prim-real [s : Symbol] [l : Real] [r : Real]) : Value
  (match s
    ['+ (NumV (+ l r))]
    ['- (NumV (- l r))]
    ['* (NumV (* l r))]
    ['/ (cond
          [(zero? r) (error '/ "DRMG division by zero, got ~e ~e" l r)]
          [else (NumV (/ l r))])]
    ['<= (BoolV (<= l r))]))

(define (prim-substring [s : String] [start : Real] [stop : Real])
  (if (and
       ( exact-nonnegative-integer? start )
       ( exact-nonnegative-integer? stop )
       (<= start stop (string-length s))
       )
   (StrV (substring s start stop)) 
    (error 'substring "DRMG bad substring arguments, got ~e ~e for ~e" start stop s)
))

(define (prim-strlen [s : String]) : Value
  [match s
    [(? string? s) (NumV (string-length s))]
    [else (error 'strlen "DRMG bad strlen arguments, got ~e, not a string " s)]
    ]
  )



; finds the parameters to substitute in the environment

(define (find-param-env [s : Symbol] [env : Env]) : Value
  (match env
    [(Env b) (match b
               [(cons b1 b2) (match b1
                               [(Binding p e) (cond
                                                [(equal? p s) e]
                                                [else (find-param-env s (Env b2))])])]
               ['() (error 'find-param-env "DRMG symbol not found")])]))


; converts values to a string, mainly for testing
(define (serialize [v : Value]) : String
  (match v
    [(CloV p b c) "#<procedure>"]
    [(PrimV o) "#<primop>"]
    [(NumV n) (~v n)]
    [(BoolV #t) "true"]
    [(StrV s) (~v s)]
    [(BoolV #f) "false"]
    ;[(StrV s) (~v s)]
    ;[(FunV p b) (~v p b)]
    ))

;test cases for serialize
(check-equal? (serialize (NumV 34)) "34")
(check-equal? (serialize (BoolV true)) "true")
(check-equal? (serialize (BoolV #f)) "false")
;(check-equal? (serialize (StrV "Hello")) "\"Hello\"")
;(check-equal? (serialize (CloV (list 'a) 'a (Env (list (Binding 'a 3))))) "#<procedure>")
;(check-equal? (serialize (PrimV '+)) "#<primop>")
;(check-equal? (serialize (FunV (list 'a) 'a)) "'(a) 'a")


;test cases for parse
(check-equal? (parse 3) 3)
(check-equal? (parse #t) #t)
(check-equal? (parse 'a) 'a)
(check-equal? (parse '{+ a 7}) (AppC '+ (list 'a 7)))
(check-equal? (parse '{- 4 7}) (AppC '- (list 4 7)))
(check-equal? (parse '{* 4 7}) (AppC '* (list 4 7)))
(check-equal? (parse '{/ 4 7}) (AppC '/ (list 4 7)))
(check-equal? (parse '{% -> 7}) (LambdaC '() 7))
(check-equal? (parse '{% a -> 7}) (LambdaC (list 'a) 7))
(check-equal? (parse '{if {<= 3 0} {+ 3 1} {- 3 1}})
              (IfC (AppC '<= (list 3 0)) (AppC '+ (list 3 1)) (AppC '- (list 3 1))))
(check-equal? (parse '{{% x y -> {+ x y}} 3 2}) (AppC (LambdaC '(x y) (AppC '+ (list 'x 'y))) '(3 2)))
(check-equal? (parse '{{% x -> x} 3}) (AppC (LambdaC '(x) 'x) '(3)))
(check-equal? (parse '{local
                        [f = {% x -> {+ x 7}}]
                        in
                        {f 4}}) (AppC (LambdaC '(f) (AppC 'f '(4)))
                                      (list (LambdaC '(x) (AppC '+ (list 'x 7))))))
(check-equal? (parse '{five}) (AppC 'five '()))
(check-exn (regexp (regexp-quote "parse: DRMG invalid symbol, got '->"))
           (lambda () (parse '{-> 3 4})))
(check-exn (regexp (regexp-quote "parse: DRMG duplicate lambda parameters, got '(a a)"))
           (lambda () (parse '{% a a -> {+ a a}})))


;test cases for interp
(check-equal? (interp (AppC '- (list 3 4)) top-env) (NumV -1))
(check-equal? (interp (AppC '* (list 3 4)) top-env) (NumV 12))
(check-equal? (interp (AppC '/ (list 12 4)) top-env) (NumV 3))
(check-equal? (interp (IfC (AppC '<= (list 1 0)) 2 0) top-env) (NumV 0))
(check-equal? (interp (IfC (AppC '<= (list -1 0)) 2 0) top-env) (NumV 2))
(check-equal? (interp #t top-env) (BoolV #t))
(check-equal? (interp (LambdaC (list 'a) 3) top-env) (CloV (list 'a) 3 top-env))

(check-exn (regexp (regexp-quote "DRMG division by zero"))
           (lambda () (interp (parse '{/ 12 0}) top-env)))

(check-exn (regexp (regexp-quote "DRMG"))
           (lambda () (interp (parse '{+ 1 "hi"}) top-env)))

(check-equal? (serialize (interp (parse "hello") top-env))  "\"hello\"")

(check-equal? (interp (parse "\"hello\"") top-env)
              (StrV "\"hello\""))

(check-equal? (interp (StrC "hi") top-env) (StrV "hi"))

(check-equal? (serialize (interp (LambdaC (list 'a) 3) top-env)) "#<procedure>")

;func within func
(check-equal? (interp (AppC (LambdaC (list 'x 'y) (AppC '+ (list 'x 'y)))
                            (list (AppC '+ (list 1 2)) 5)) top-env) (NumV 8))

(check-equal? (interp (AppC (LambdaC (list 'x 'y) (AppC '+ (list 'x 'y)))
                            (list 3 4)) top-env) (NumV 7))

(check-exn
 (regexp (regexp-quote "interp: DRMG inequal parameters and arguments"))
 (lambda () (top-interp '{{% x -> {+ x 2}} 3 4 5})))

(check-equal? (interp (AppC (LambdaC (list 'x) (AppC 'x (list 4)))
                            (list (LambdaC (list 'a) (AppC '+ (list 'a 7))))) top-env)
              (NumV 11))

(check-exn
 (regexp(regexp-quote "find-param-env: DRMG symbol not found"))
 (lambda () (interp (AppC (LambdaC (list 'x 'y) (AppC '+ (list 'x 'z)))
                            (list 3 4)) top-env)))

;top interp
(check-equal? (top-interp '{{% a -> {+ a 7}} 3}) "10")
(check-equal? (top-interp '{{% a -> {true}} 3}) "true")
(check-equal? (top-interp '{{% a -> {false}} 3}) "false")
(check-equal? (top-interp '{{% a -> {if {true} a {false}}} 3}) "3")
(check-equal? (serialize (PrimV '+)) "#<primop>")
(check-equal? (top-interp '{{% a b -> {+ a b}} 3 7}) "10")
(check-equal? (top-interp '{{% x -> {x 4}} {% a -> {+ a 7}}}) "11")
(check-equal? (top-interp '{local 
                             [f = {% x -> {+ {* x x} {+ {* 4 x} 4}}}]
                             in
                             {f 7}}) "81")
(check-equal? (top-interp '{local
                             [pow = {% a b pow -> {if {<= b 0} 1 {* a {pow a {- b 1} pow}}}}]
                             in
                             {pow 12 3 pow}}) "1728")
(check-equal? (top-interp '{{% add -> {{% one -> {{% two -> {{% three -> 
                                                                {{three {% x -> {* x 2}}} 2}}
                                                             {{add one} two}}}
                                                  {% f -> {% a -> {f {f a}}}}}}
                                       {% f -> {% a -> {f a}}}}}
                            {% n1 -> {% n2 -> {% f -> {% a -> {{n1 f} {{n2 f} a}}}}}}}) "16")
(check-equal? (top-interp '{substring "hello" 1 3}) "\"el\"")
(check-equal? (top-interp '{strlen "hello"}) "5")

