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

(define-type Result
  [v*s (v Value?) (s Store?)])

(define-type Value 
  [numV (n number?)]
  [boolV (b boolean?)]
  [closV (arg symbol?) (body ExprC?) (env Env?)])

;; Locations
(define-type Location
  [loc (n number?)])

; Defining bindings and environment
(define-type Binding
  [bound (name symbol?) (val Location?)])

(define-type Env
  [mt-env]
  [extend-env (with Binding?) (rest Env?)])


;; Storage
(define-type Storage 
  [cell (loc Location?) (val Value?)])

;; Store
(define-type Store
  [mt-store]
  [override-store (with Storage?) (rest Store?)])

; ExprC core language representation
(define-type ExprC
  [numC (n number?)]
  [boolC (e boolean?)]
  [idC (s symbol?)]
  [appC (fun ExprC?) (arg ExprC?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)]
  [ifC (test ExprC?) (t ExprC?) (f ExprC?)]
  [lamC (arg symbol?) (body ExprC?)]
  [setC (var symbol?) (arg ExprC?)]
  [seqC (b1 (or/c ExprC? void?)) (b2 ExprC?)])

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
  [letS (what symbol?) (to ExprS?) (in ExprS?)]
  [setS (var symbol?) (arg ExprS?)]
  [seqS (arg (listof ExprS?))])

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
                               (desugar actual))]
      [setS (v a) (setC v (desugar a))]
      [seqS (loe) (cond 
                    [(empty? loe) (error 'desugar "seqS must have at least one expr")]
                    [(= (length loe) 1) (seqC (void 0) (desugar (first loe)))]
                    [(= (length loe) 2) (seqC (desugar (first loe))
                                              (desugar (second loe)))]
                    [else (seqC (desugar (first loe)) 
                                (desugar (seqS (rest loe))))])])))

;;; INTERPRETER ;;;
; Symbol Env -> Location or (error "unbound id")
(define lookup
  (λ (what in)
    (type-case Env in
      [mt-env () (error ":unbound identifier")]
      [extend-env (binding rst) 
                  (cond 
                    [(symbol=? what (bound-name binding)) (bound-val binding)]
                    [else (lookup what rst)])])))

;; Location Store -> Value
; lookup the value in Store when associated with the given location
(define fetch
  (λ (what in)
    (type-case Store in
      [mt-store () (error ":location not found")]
      [override-store (cell rst) 
                      (cond 
                        [(= (loc-n what) (loc-n (cell-loc cell))) (cell-val cell)]
                        [else (fetch what rst)])])))

; num+ : numV numV -> numV
; num* : numV numV -> numV
(define num-lr
  (λ (op l r)
    (cond
      [(and (numV? l) (numV? r)) (numV (op (numV-n l) (numV-n r)))]
      [else (error (format "~s: argument not a numV" op))])))

(define (num+ l r) (num-lr + l r))
(define (num* l r) (num-lr * l r))

;; new-loc: -> Location
;; produce a new unused location
(define new-loc
  (let  ((n (box (loc 0))))
    (λ () 
      (begin
        (set-box! n (loc (add1 (loc-n (unbox n)))))
        (unbox n)))))

; ExprC -> Result
(define interpret 
  (λ (ex) 
    (interp ex (mt-env) (mt-store))))

; ExprC Env Store -> Result
(define interp 
  (λ (ex env sto)
    (type-case ExprC ex
      [numC (n) (v*s (numV n) sto)]
      [boolC (e) (v*s (boolV e) sto)]
      [idC (s) (v*s (fetch (lookup s env) sto) sto)]
      [lamC (a b) (v*s (closV a b env) sto)]
      [appC (f a) (type-case Result (interp f env sto)
                    [v*s (v-f s-f) 
                         (type-case Result (interp a env s-f)
                           [v*s (v-a s-a) 
                                (let ((where (new-loc)))
                                  (interp (closV-body v-f)
                                          (extend-env (bound (closV-arg v-f)
                                                             where)
                                                      (closV-env v-f))
                                          (override-store (cell where v-a)
                                                          s-a)))])])]                                               
      [plusC (l r) (type-case Result (interp l env sto)
                     [v*s (v-l s-l) 
                          (type-case Result (interp r env s-l)
                            [v*s (v-r s-r) 
                                (v*s (num+ v-l v-r) s-r)])])]
      [multC (l r) (type-case Result (interp l env sto)
                     [v*s (v-l s-l) 
                          (type-case Result (interp r env s-l)
                            [v*s (v-r s-r) 
                                (v*s (num* v-l v-r) s-r)])])]
      [ifC (test t f) 
           (type-case Result (interp test env sto)
             (v*s (v-test s-test)                  
                  (cond ;only (boolV #f) leads to f branch, everything else is true
                    [(and (boolV? v-test) 
                          (not (boolV-b v-test))) (interp f env s-test)]
                    [else (interp t env s-test)])))]
      [setC (var val) (type-case Result (interp val env sto)
                        [v*s (v-val s-val) 
                             (let ((where (lookup var env)))
                               (v*s v-val 
                                    (override-store (cell where v-val)
                                                    s-val)))])]
      [seqC (b1 b2) (cond
                      [(void? b1) (interp b2 env sto)]
                      [else (type-case Result (interp b1 env sto)
                              [v*s (v-b1 s-b1) (interp b2 env s-b1)])])])))


;;; TESTS ;;;
(abridged-test-output true)
;(print-only-errors true)

;; Result -> (or/c number? boolean? string?)
;; for testing purpouses return the actual value of interp
(define get-value 
  (λ (r) 
    (type-case Result r
      [v*s (v-r s-r)            
           (type-case Value v-r
             [numV (n) n]
             [boolV (b) b]
             [closV (f b e) "closure"])])))
             
;; boxes
(test (get-value (interpret (desugar (letS 'a (plusS (numS 1) (numS 0))
                                           (letS 'b (numS 2)
                                             (plusS (idS 'a) (idS 'b)))))))
      3)

(test (get-value 
       (interpret
        (desugar
         (letS 'b (numS 0) 
               (seqS (list (setS 'b (numS 1))
                           (idS 'b)))))))
      1)
(test (get-value 
       (interpret 
        (desugar                             
         (letS 'b (numS 0)
               (letS 'incr (lamS 'x (plusS (idS 'x) (numS 1)))
                     (ifS (boolS false)
                          (idS 'b)
                          (seqS (list (setS 'b (appS (idS 'incr)
                                                     (idS 'b)))
                                      (idS 'b)))))))))
      1)

; ; parsed and desugared test cases
; (define pnum (desugar (parse num)))
; (define ppl (desugar (parse pl)))
; (define pmul (desugar (parse mul)))
; (define pbmin (desugar (parse bmin)))
; (define pumin (desugar (parse umin)))
; (define ptbool (desugar (parse tbool)))
; (define pfbool (desugar (parse fbool)))
; (define pift (desugar (parse ift)))
; (define piff (desugar (parse iff)))
; 
; (printf "~n--- Testing ---\n")
; 
; (printf "\n--- basic arithmetic\n")
; (test (interp pnum (mt-env)) (numV 5))
; (test (interp ppl (mt-env)) (numV 10))
; (test (interp pmul (mt-env)) (numV 50))
; (test (interp pbmin (mt-env)) (numV 6))
; (test (interp pumin (mt-env)) (numV 1))
; 
; (printf "\n--- booleans and if\n")
; (test (interp ptbool (mt-env)) (boolV true))
; (test (interp pfbool (mt-env)) (boolV false))
; (test (interp pift (mt-env)) (boolV true))
; (test (interp piff (mt-env)) (boolV false))
;   
; 
; (printf "\n--- identity function\n")
; (test (interp (appC (lamC 'x
;                           (idC 'x))
;                     (numC 6)) (mt-env)) (numV 6))
; 
; (printf "\n--- nested applications\n")
; ;check that outer bindings preserved in nested applications
; ;(λ (x) ((λ (y) (+ x y)) 4) 5)
; (test (interp (appC (appC (lamC 'x
;                         (lamC 'y
;                               (plusC (idC 'x) (idC 'y))))
;                   (numC 4))
;             (numC 5)) (mt-env)) (numV 9))
; 
; (printf "\n--- capture-free substitution\n")
; ;interp with env and closures solves this problem automagically
; ;(((λ (f) (λ (x) (f 10))) (λ (y) (+ x y))) 5)
; (test/exn (interp (appC (appC (lamC 'f 
;                           (lamC 'x
;                                 (appC (idC 'f) (numC 10))))
;                     (lamC 'y
;                           (plusC (idC 'x) (idC 'y))))
;                     (numC 5))
;               (mt-env))
;       "unbound identifier")
; 
; (printf "\n--- desugaring let\n")
; ;(let ((double (λ (x) (+ x x)))) (double 10))
; (test (interp (desugar 
;                (letS 'double 
;                      (lamS 'x (plusS (idS 'x) (idS 'x)))
;                      (appS (idS 'double) (numS 10))))
;               (mt-env))
;       (numV 20))
; 
; ;(let ((double (λ (x) (+ x x))))
; ; (let ((quad (λ (x) (double (double x)))))
; ;  (quad 10)))
; (test (interp (desugar
;                (letS 'double
;                      (lamS 'x (plusS (idS 'x) (idS 'x)))
;                      (letS 'quad 
;                            (lamS 'x (appS (idS 'double)
;                                           (appS (idS 'double) (idS 'x))))
;                            (appS (idS 'quad) (numS 10)))))
;               (mt-env))
;       (numV 40))