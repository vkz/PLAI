#lang plai
;;; TEST DATA ;;;
; unparsed test cases
; arithmetic
(define-values 
  (err num pl mul bmin umin) 
  (values '(&err 3 4) 5 '(+ 3 7) '(* 5 (+ 3 7)) '(- 10 4) '(- (- 1))))

; conditionals
(define-values
  (tbool fbool ift iff)
  (values
   true false (list 'if '(- 2 5) true false) (list 'if false true false)))


;;; DATA ;;;
; Result of interpretation is either a number or a boolean:
; (λ(x) (or (number? x) (boolean? x)))

; Defining bindings and environment
(define-type Binding
  [bound (name symbol?) (val (λ(x) (or (number? x) (boolean? x))))])

(define-type Env
  [mt-env]
  [extend-env (with Binding?) (rest Env?)])

; Defining functions
(define-type FunDefC 
  [fdC (name symbol?) (arg symbol?) (body ExprC?)])

; ExprC core language representation
(define-type ExprC
  [numC (n number?)]
  [boolC (e boolean?)]
  [idC (s symbol?)]
  [appC (fun symbol?) (arg ExprC?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)]
  [ifC (test ExprC?) (t ExprC?) (f ExprC?)])

; ExprS extended language representation
(define-type ExprS
  [numS (n number?)]
  [boolS (e boolean?)]
  [idS (s symbol?)]
  [appS (fun symbol?) (arg ExprS?)]
  [plusS (l ExprS?) (r ExprS?)]
  [multS (l ExprS?) (r ExprS?)]
  [uminusS (e ExprS?)]
  [bminusS (l ExprS?) (r ExprS?)]
  [ifS (test ExprS?) (t ExprS?) (f ExprS?)])

;;; PARSER ;;;
; Racket s-expr -> ExprS
(define parse 
  (λ (e) 
    (cond 
      [(number? e) (numS e)]
      [(boolean? e) (boolS e)]
      [(symbol? e) (idS e)]
      [(list? e)
       (case (first e)
         [(+) (plusS (parse (second e)) (parse (third e)))]
         [(*) (multS (parse (second e)) (parse (third e)))]
         [(-) (if (> (length e) 2) 
                  (bminusS (parse (second e)) (parse (third e)))
                  (uminusS (parse (second e))))]
         [(if) (ifS (parse (second e))
                      (parse (third e))
                      (parse (fourth e)))]
         [else (cond 
                 [(symbol? (first e)) (appS (first e)
                                            (parse (second e)))]
                 [else (error ":function name in application must be a symbol")])])]
      [else (error ":unrecognized datatype")])))

;;; DESUGAR ;;;
; minus-helper 
; for a-b = a + -1*b
; for unary -b = 0 + -1*b
(define minus-helper
  (λ (ac bc)
    (plusC ac (multC (numC -1) bc))))

; ExprS -> ExprC
(define desugar
  (λ (as)
    (type-case ExprS as
      [numS (n) (numC n)]
      [boolS (e) (boolC e)]
      [idS (s) (idC s)]
      [appS (f e) (appC f (desugar e))]
      [plusS (l r) (plusC (desugar l) (desugar r))]
      [multS (l r) (multC (desugar l) (desugar r))]
      [bminusS (l r) (minus-helper (desugar l) (desugar r))]
      [uminusS (e) (minus-helper (numC 0) (desugar e))]
      [ifS (test t f) (ifC (desugar test)
                           (desugar t)
                           (desugar f))])))

;;; INTERPRETER ;;;
; symbol (listof FunDefC) -> FunDefC
(define get-fundef
  (λ (s fds)
    (cond 
      [(empty? fds) (error (format ":unknown function ~v" s))]
      [(cons? fds) (cond 
                     [(symbol=? s (fdC-name (first fds))) (first fds)]
                     [else (get-fundef s (rest fds))])])))
      

; ExprC symbol ExprC -> ExprC
(define subst
  (λ (what for in)
    (type-case ExprC in
      [numC (n) in]
      [boolC (e) in]
      [idC (s) (if (symbol=? s for)
                   what
                   in)]
      [appC (f a) (appC f (subst what for a))]
      [plusC (l r) (plusC (subst what for l)
                          (subst what for r))]
      [multC (l r) (multC (subst what for l)
                          (subst what for r))]
      [ifC (test t f) (ifC (subst what for test)
                           (subst what for t)
                           (subst what for f))])))

; number? or boolean? -> ExprC
; wraps the result of interpretation into ExprC so it can be fed to interp again
; need this wrapper for eager eval of arg in function applications
(define val->ExprC
  (λ (a)
    (cond 
      [(number? a) (numC a)]
      [(boolean? a) (boolC a)]
      [else (error ":can't produce ExprC not number or boolean")])))

; Symbol Env -> ExprC or (error "unbound id")
(define lookup
  (λ (what in)
    (type-case Env in
      [mt-env () (error ":unbound identifier")]
      [extend-env (binding rst) 
                  (cond 
                    [(symbol=? what (bound-name binding)) (bound-val binding)]
                    [else (lookup what rst)])])))

; ExprC Env (listof FunDefC) -> (or number? boolean?)
(define interp 
  (λ (ex env fds)
    (type-case ExprC ex
      [numC (n) n]
      [boolC (e) e]
      [idC (s) (lookup s env)]
      [appC (f a) (local ([define fd (get-fundef f fds)]
                          [define a-interped (interp a env fds)])
                    (interp (fdC-body fd)
                            (extend-env (bound (fdC-arg fd) a-interped)
                                        (mt-env)) ;replace with env to get dynamic scope
                            fds))]
      [plusC (l r) (+ (interp l env fds) (interp r env fds))]
      [multC (l r) (* (interp l env fds) (interp r env fds))]
      [ifC (test t f) 
           (local ([define test-result (interp test env fds)])
             (if test-result
                 (interp t env fds)
                 (interp f env fds)))])))

;;; TESTS ;;;
(abridged-test-output true)

; parsed and desugared test cases
(define pnum (desugar (parse num)))
(define ppl (desugar (parse pl)))
(define pmul (desugar (parse mul)))
(define pbmin (desugar (parse bmin)))
(define pumin (desugar (parse umin)))
(define ptbool (desugar (parse tbool)))
(define pfbool (desugar (parse fbool)))
(define pift (desugar (parse ift)))
(define piff (desugar (parse iff)))

(printf "~n--- Testing ---")

(printf "\nBasic arithmetic\n")
(test (interp pnum (mt-env)'()) 5)
(test (interp ppl (mt-env) '()) 10)
(test (interp pmul (mt-env) '()) 50)
(test (interp pbmin (mt-env) '()) 6)
(test (interp pumin (mt-env) '()) 1)

(printf "\nBooleans and if\n")
(test (interp ptbool (mt-env) '()) true)
(test (interp pfbool (mt-env) '()) false)
(test (interp pift (mt-env) '()) true)
(test (interp piff (mt-env) '()) false)
  
(printf "\nFunctions with substitution\n")
(define pfinc (fdC 'incr 'x (desugar (parse '(+ x 1)))))
(define pfaddxy (fdC 'addxy 'x (desugar (parse '(+ x y))))) ;unbound 'y
(define lof (list pfinc pfaddxy))

(test (interp (desugar (parse (list 'incr '(+ 3 (incr 5))))) (mt-env) lof) 10) ;(incr (+ 3 (incr 5)))
(test/exn (interp (desugar (parse (list 'nofun 5))) (mt-env) lof) ":unknown function") ;(nufun 5)
(test/exn (interp (desugar (parse (list 'addxy 5))) (mt-env) lof) ":unbound identifier") ;(addxy 5) 

;(define (f1 x) (f2 4))
;(define (f2 y) (+ x y))
;(f1 3)
; good when static scope
; bad when dynamic scope
(test/exn (interp (appC 'f1 (numC 3))
                  (mt-env)
                  (list (fdC 'f1 'x (appC 'f2 (numC 4)))
                        (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))
          ":unbound identifier") 

(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              (mt-env)
              (list (fdC 'const5 '_ (numC 5))))
      15)

(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              (mt-env)
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)

(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              (mt-env)
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)

 