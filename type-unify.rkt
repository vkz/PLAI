#lang plai-typed

(require "type-constraints.rkt")

(define-type-alias Subst (listof Substitution))

(define-type Substitution
  [sub (var : Term) (is : Term)])

(define (unify [cs : (listof Constraints)]) : Subst
  (unify/theta cs empty))

(define (lookup [t : Term] [in : Subst]) : (optionof Term)
  (cond
    [(empty? in) (none)]
    [(cons? in) 
     (type-case Substitution (first in)
       [sub (var is) (if (eq? var t)
                         (some is)
                         (lookup t (rest in)))])]))

(define (extend+replace [l : Term]
                        [r : Term]
                        [s : Subst]) : Subst  
  (local ([define (l-occurs-in? [rhs : Term]) : boolean
            (type-case Term r
              [tArrow (dom rng) (or (l-occurs-in? dom)
                                    (l-occurs-in? rng))]
              [else (eq? l rhs)])]
          
          [define (l-replace-in [s : Subst]) : Subst
            (cond
              [(empty? s) empty]
              [(cons? s)
               (let ([var (sub-var (first s))]
                     [is (replace (sub-is (first s)))])
                 (cons (sub var is)
                       (l-replace-in (rest s))))])]
          
          [define (replace [t : Term]) : Term
            (type-case Term t
              [tArrow (dom rng) (tArrow (replace dom)
                                        (replace rng))]
              [else (if (eq? t l)
                        r
                        t)])])
    (cond
      [(not (l-occurs-in? r)) (cons (sub l r)
                                    (l-replace-in s))]
      [else (error 'extend+replace "cycle in substitution")])))

(define (unify/theta [cs : (listof Constraints)] [theta : Subst]) : Subst
  (begin 
    (display "\nNew cs\n")
    (display cs)
    (display "\n\n")
  (cond 
    [(empty? cs) theta]
    [(cons? cs) 
     (let ([l (eqCon-lhs (first cs))]
           [r (eqCon-rhs (first cs))])
       (type-case Term l
         [tVar (s) (type-case (optionof Term) (lookup l theta)
                     [some (bound)
                           (unify/theta (cons (eqCon bound r)
                                              (rest cs))
                                        theta)]
                     [none () (unify/theta (rest cs)
                                           (extend+replace l r theta))])]
         
         [tExp (e) (type-case (optionof Term) (lookup l theta)
                     [some (bound)
                           (unify/theta (cons (eqCon bound r)
                                              (rest cs))
                                        theta)]
                     [none () (unify/theta (rest cs)
                                           (extend+replace l r theta))])]
         
         [tNum () (type-case Term r
                    [tNum () (unify/theta (rest cs) theta)]
                    [else (error 'unify "number and something else")])]
         
         [tArrow (d1 r1) (type-case Term r
                           [tArrow (d2 r2) 
                                   (unify/theta (cons (eqCon d1 d2)
                                                      (cons (eqCon r1 r2)
                                                            (rest cs)))
                                                theta)]
                           [else (error 'unify "arrow and something else")])]))])))

         


;(unify (cg (appC (lamC 'x (appC (idC 'x) (numC 1))) (lamC 'y (plusC (idC 'y) (numC 0))))))

(unify (cg (lamC 'x (idC 'x))))