#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [appC (fun : ExprC) (arg : ExprC)])

(define-type Constraints
  [eqCon (lhs : Term) (rhs : Term)])

(define-type Term
  [tNum]
  [tVar (s : symbol)]
  [tExp (e : ExprC)]
  [tArrow (dom : Term) (rng : Term)])

(define (append3 [a : (listof 'a)]
                 [b : (listof 'a)]
                 [c : (listof 'a)]) : (listof 'a)
  (append a (append b c)))


(define (cg [e : ExprC]) : (listof Constraints)
  (type-case ExprC e
    [numC (_) (list (eqCon (tExp e) (tNum)))]
    [idC (s) (list (eqCon (tExp e) (tVar s)))]
    [plusC (l r) (append3 (cg l)
                          (cg r)
                          (list (eqCon (tExp l) (tNum))
                                (eqCon (tExp r) (tNum))
                                (eqCon (tExp e) (tNum))))]
    [lamC (a b) (append (cg b)
                        (list (eqCon (tExp e) (tArrow (tVar a) (tExp b)))))]
    [appC (f a) (append3 (cg f)
                         (cg a)
                         (list (eqCon (tExp f) (tArrow (tExp a) (tExp e)))))]))
    


              