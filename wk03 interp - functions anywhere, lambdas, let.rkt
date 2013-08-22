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
; Result of interpretation is Value

(define-type Value 
  [numV (n number?)]
  [boolV (b boolean?)]
  [closV (arg symbol?) (body ExprC?) (env Env?)])

; Defining bindings and environment
(define-type Binding
  [bound (name symbol?) (val Value?)])

(define-type Env
  [mt-env]
  [extend-env (with Binding?) (rest Env?)])

; ExprC core language representation
(define-type ExprC
  [numC (n number?)]
  [boolC (e boolean?)]
  [idC (s symbol?)]
  [appC (fun ExprC?) (arg ExprC?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)]
  [ifC (test ExprC?) (t ExprC?) (f ExprC?)]
  [lamC (arg symbol?) (body ExprC?)])

; ExprS extended language representation
(define-type ExprS
  [numS (n number?)]
  [boolS (b boolean?)]
  [idS (s symbol?)]
  [appS (fun ExprS?) (arg ExprS?)]
  [plusS (l ExprS?) (r ExprS?)]
  [multS (l ExprS?) (r ExprS?)]
  [uminusS (e ExprS?)]
  [bminusS (l ExprS?) (r ExprS?)]
  [ifS (test ExprS?) (t ExprS?) (f ExprS?)]
  [lamS (arg symbol?) (body ExprS?)]
  [letS (what symbol?) (to ExprS?) (in ExprS?)])

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
      [appS (f e) (appC (desugar f) (desugar e))]
      [plusS (l r) (plusC (desugar l) (desugar r))]
      [multS (l r) (multC (desugar l) (desugar r))]
      [bminusS (l r) (minus-helper (desugar l) (desugar r))]
      [uminusS (e) (minus-helper (numC 0) (desugar e))]
      [ifS (test t f) (ifC (desugar test)
                           (desugar t)
                           (desugar f))]
      [lamS (a b) (lamC a (desugar b))]
      [letS (arg actual body) (appC (lamC arg (desugar body))
                               (desugar actual))])))

;;; INTERPRETER ;;;
; Symbol Env -> Value or (error "unbound id")
(define lookup
  (λ (what in)
    (type-case Env in
      [mt-env () (error ":unbound identifier")]
      [extend-env (binding rst) 
                  (cond 
                    [(symbol=? what (bound-name binding)) (bound-val binding)]
                    [else (lookup what rst)])])))

; num+ : numV numV -> numV
; num* : numV numV -> numV
(define num-lr
  (λ (op l r)
    (cond
      [(and (numV? l) (numV? r)) (numV (op (numV-n l) (numV-n r)))]
      [else (error (format "~s: argument not a numV" op))])))

(define (num+ l r) (num-lr + l r))
(define (num* l r) (num-lr * l r))


; ExprC Env -> Value
(define interp 
  (λ (ex env)
    (type-case ExprC ex
      [numC (n) (numV n)]
      [boolC (e) (boolV e)]
      [idC (s) (lookup s env)]
      [lamC (a b) (closV a b env)]
      [appC (f a) (local ([define cl (interp f env)]
                          [define a-interped (interp a env)])
                    (cond 
                      [(closV? cl)
                       (interp (closV-body cl)
                               (extend-env (bound (closV-arg cl) a-interped)
                                           (closV-env cl)))] ;replace with env to get dynamic scope
                      [else (error "application: function position has to evaluate to closV")]))]
      [plusC (l r) (num+ (interp l env) (interp r env))]
      [multC (l r) (num* (interp l env) (interp r env))]
      [ifC (test t f) 
           (local ([define test-value (interp test env)])
             (cond ;only (boolV #f) leads to f branch, everything else is true
               [(and (boolV? test-value) (not (boolV-b test-value))) 
                 (interp f env)]
               [else (interp t env)]))])))


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

(printf "~n--- Testing ---\n")

(printf "\n--- basic arithmetic\n")
(test (interp pnum (mt-env)) (numV 5))
(test (interp ppl (mt-env)) (numV 10))
(test (interp pmul (mt-env)) (numV 50))
(test (interp pbmin (mt-env)) (numV 6))
(test (interp pumin (mt-env)) (numV 1))

(printf "\n--- booleans and if\n")
(test (interp ptbool (mt-env)) (boolV true))
(test (interp pfbool (mt-env)) (boolV false))
(test (interp pift (mt-env)) (boolV true))
(test (interp piff (mt-env)) (boolV false))
  

(printf "\n--- identity function\n")
(test (interp (appC (lamC 'x
                          (idC 'x))
                    (numC 6)) (mt-env)) (numV 6))

(printf "\n--- nested applications\n")
;check that outer bindings preserved in nested applications
;(λ (x) ((λ (y) (+ x y)) 4) 5)
(test (interp (appC (appC (lamC 'x
                        (lamC 'y
                              (plusC (idC 'x) (idC 'y))))
                  (numC 4))
            (numC 5)) (mt-env)) (numV 9))

(printf "\n--- capture-free substitution\n")
;interp with env and closures solves this problem automagically
;(((λ (f) (λ (x) (f 10))) (λ (y) (+ x y))) 5)
(test/exn (interp (appC (appC (lamC 'f 
                          (lamC 'x
                                (appC (idC 'f) (numC 10))))
                    (lamC 'y
                          (plusC (idC 'x) (idC 'y))))
                    (numC 5))
              (mt-env))
      "unbound identifier")

(printf "\n--- desugaring let\n")
;(let ((double (λ (x) (+ x x)))) (double 10))
(test (interp (desugar 
               (letS 'double 
                     (lamS 'x (plusS (idS 'x) (idS 'x)))
                     (appS (idS 'double) (numS 10))))
              (mt-env))
      (numV 20))

;(let ((double (λ (x) (+ x x))))
; (let ((quad (λ (x) (double (double x)))))
;  (quad 10)))
(test (interp (desugar
               (letS 'double
                     (lamS 'x (plusS (idS 'x) (idS 'x)))
                     (letS 'quad 
                           (lamS 'x (appS (idS 'double)
                                          (appS (idS 'double) (idS 'x))))
                           (appS (idS 'quad) (numS 10)))))
              (mt-env))
      (numV 40))