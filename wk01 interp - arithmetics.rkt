#lang plai
; unparsed test cases
(define-values 
  (err num pl mul bmin umin) 
  (values '(&err 3 4) 5 '(+ 3 7) '(* 5 (+ 3 7)) '(- 10 4) '(- (- 1))))

;;; DATA ;;;
; core arithmetic language representation
(define-type ArithC
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)])

; extended arithmetic language representation
(define-type ArithS
  [numS (n number?)]
  [plusS (l ArithS?) (r ArithS?)]
  [multS (l ArithS?) (r ArithS?)]
  [uminusS (e ArithS?)]
  [bminusS (l ArithS?) (r ArithS?)])


;;; PARSER ;;;
(define parse 
  (λ (e) 
    (cond 
      [(number? e) (numS e)]
      [(list? e)
       (case (first e)
         [(+) (plusS (parse (second e)) (parse (third e)))]
         [(*) (multS (parse (second e)) (parse (third e)))]
         [(-) (if (> (length e) 2) 
                  (bminusS (parse (second e)) (parse (third e)))
                  (uminusS (parse (second e))))]
         [else (error ":unrecognized op by parser")])])))

; parse should throw an exception when given urecognized op
(test/exn (parse err) "by parser")

; minus-helper 
; for a-b = a + -1*b
; for unary -b = 0 + -1*b
(define minus-helper
  (λ (ac bc)
    (plusC ac (multC (numC -1) bc))))

;;; DESUGARAR ;;;
; desugaring ArithS -> ArithC
(define desugar
  (λ (as)
    (type-case ArithS as
      [numS (n) (numC n)]
      [plusS (l r) (plusC (desugar l) (desugar r))]
      [multS (l r) (multC (desugar l) (desugar r))]
;      [bminusS (l r) (plusC (desugar l) 
;                            (multC (numC -1) (desugar r)))]
      [bminusS (l r) (minus-helper (desugar l) (desugar r))]
      ;[uminusS (e) (desugar (bminusS (numS 0) e))])))
      [uminusS (e) (minus-helper (numC 0) (desugar e))])))

; parsed and desugared test cases
(define pnum (desugar (parse num)))
(define ppl (desugar (parse pl)))
(define pmul (desugar (parse mul)))
(define pbmin (desugar (parse bmin)))
(define pumin (desugar (parse umin)))

  
;;; INTERPRETER ;;;
(define interp 
  (λ (a)
    (type-case ArithC a
      [numC (n) n]
      [plusC (l r) (+ (interp l) (interp r))]
      [multC (l r) (* (interp l) (interp r))])))

; numC -> number?
; plusC -> number?
; multC -> number?
(test (interp pnum) 5)
(test (interp ppl) 10)
(test (interp pmul) 50)
(test (interp pbmin) 6)
(test (interp pumin) 1)
  
  


