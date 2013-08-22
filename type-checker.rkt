#lang plai-typed

(define-type Type 
  [numT]
  [boolT]
  [funT (arg : Type) (ret : Type)])

(define-type-alias TyEnv (listof Binding))
(define extend-ty-env cons)

(define-type Binding
  [bound (name : symbol) (type : Type)])

(define bind bound)

(define lookup
  (lambda ([id : symbol]
           [env : TyEnv])
    (cond
      [(empty? env) 
       (error id "Unbound identifier: ")]
      
      [(symbol=? id (bound-name (first env))) 
       (bound-type (first env))]
      
      [else (lookup id (rest env))])))

(define-type TyExprC
  [numC (n : number)]
  [boolC (n : boolean)]
  [idC (s : symbol)]
  [ifC (test : TyExprC) (th : TyExprC) (el : TyExprC)]
  [appC (fun : TyExprC) (arg : TyExprC)]
  [plusC (l : TyExprC) (r : TyExprC)]
  [multC (l : TyExprC) (r : TyExprC)]
  [lamC (arg : symbol) (argT : Type) (retT : Type) (body : TyExprC)]
  [recC (f : symbol) (a : symbol) (aT : Type) (rT : Type) (b : TyExprC) (u : TyExprC)])



(define (tc [expr : TyExprC] [tenv : TyEnv]) : Type
  (type-case TyExprC expr
    [numC (n) (numT)]
    
    [boolC (n) (boolT)]
    
    [idC (n) (lookup n tenv)]
    
    [plusC (l r) (let ([lt (tc l tenv)]
                       [rt (tc r tenv)])
                   (if (and (numT? lt)
                            (numT? rt))
                       (numT)
                       (error 'tc "+ not both numbers")))]
    
    [multC (l r) (let ([lt (tc l tenv)]
                       [rt (tc r tenv)])
                   (if (and (numT? lt)
                            (numT? rt))
                       (numT)
                       (error 'tc "* not both numbers")))]
    
    [appC (f a) (let ([ft (tc f tenv)]
                      [at (tc a tenv)])
                  (cond
                    [(not (funT? ft))
                     (error 'tc "not a function")]
                    [(not (equal? (funT-arg ft) at))
                     (error 'tc "app arg mismatch")]
                    [else (funT-ret ft)]))]
    
    [lamC (a argT retT b)
          (if (equal? (tc b (extend-ty-env (bind a argT) tenv)) 
                      retT)
              (funT argT retT)
              (error 'lam "lam type mismatch"))]
    
    [recC (f a aT rT b u)
          (let ([extended-env (extend-ty-env (bind f (funT aT rT)) tenv)])
            (cond 
              [(not (equal? rT 
                            (tc b 
                                (extend-ty-env (bind a aT) 
                                               extended-env))))
               (error 'tc "body return type not correct")]
              [else (tc u extended-env)]))]                
                
    [ifC (t th el) 
         (let ([tht (tc th tenv)]
               [elt (tc el tenv)])
           (cond 
             [(not (boolT? (tc t tenv))) 
              (error 'tc "not boolean in test clause of if")]
             [(not (equal? tht elt)) 
              (error 'tc "type mismatch in if")]
             [else tht]))]))
              