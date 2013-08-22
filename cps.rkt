#lang plai-typed

(define table (make-hash empty))

;; produce a new label
(define new-label
  (let ([lcounter 0])
    (lambda ()
      (begin
        (set! lcounter (add1 lcounter))
        lcounter))))

;; use the label in action field to get the rest of computation and pass it the value 
;; tantamaunt to clicking submit
(define (resume g n)
  ((some-v (hash-ref table g)) n))

;; store the rest of computation and "produce the page"
(define (read-number/suspend prompt rest)
  (let ([g (new-label)])
    (begin
      (hash-set! table g rest)
      (display prompt)
      (display " To enter it, use the action field label ")
      (display g))))


(define-syntax (cps e)
  (syntax-case e (let/cc try/cc generator with rec lam cnd seq set quote display read-number) 
    
    [(_ (let/cc kont b))
     (identifier? #'kont)
     #'(lambda (k)
         (let ([kont (lambda (v dyn-k)
                       (k v))])
           ((cps b) k)))]
    
    ;; try-throw exception mechanism
    [(_ (try/cc throw b))
     #'(cps (let/cc throw b))] 
    
    [(_ (generator (yield) (v) b))
     (and (identifier? #'v) (identifier? #'yield))
     #'(lambda (k)
         (k (let ([where-to-go (lambda (v) (error 'where-to-go "nothing"))])
              (letrec([resumer (lambda (v)
                                 ((cps b) (lambda (k)
                                            (error 'generator "fell through"))))]
                      [yield (lambda (v gen-k)
                               (begin
                                 (set! resumer gen-k)
                                 (where-to-go v)))])
                (lambda (v dyn-k)
                  (begin
                    (set! where-to-go dyn-k)
                    (resumer v)))))))]
                
    [(_ (with (v e) b))
     #'(cps ((lam (v) b) e))]
    
    [(_ (rec (v f) b))
     #'(cps (with (v (lam (arg) (error 'dummy "nothing")))
                  (seq 
                   (set v f)
                   b)))]
    
    [(_ (lam (a) b))
     (identifier? #'a)
     #'(lambda (k)
         (k (lambda (a dyn-k)
              ((cps b) dyn-k))))]
    
    [(_ (lam () b))
     #'(lambda (k) 
         (k (lambda (dyn-k)
              ((cps b) dyn-k))))]
    
    
    [(_ (cnd tst thn els))
     #'(lambda (k) 
         ((cps tst) (lambda (tstv)
                      (if tstv
                          ((cps thn) k)
                          ((cps els) k)))))]
    
    [(_ (seq e1 e2))
     #'(lambda (k) 
         ((cps e1) (lambda (_)
                     ((cps e2) k))))]
        
   
     [(_ (set v e))
     #'(lambda (k)
         ((cps e) (lambda (ev) 
                    (k (set! v ev)))))]
    
    [(_ 'a)
     #'(lambda (k)
         (k 'a))]
    
    [(_ (display output))
     #'(lambda (k)
         ((cps output) (lambda (ov)
                         (k (display ov)))))]
            
    [(_ (read-number prompt))
     #'(lambda (k)
         ((cps prompt) (lambda (pv)
                         (read-number/suspend pv k))))]
    
    
    ;; user function application case
    [(_ (f a))
     #'(lambda (k) 
         ((cps f) (lambda (fv)
                    ((cps a) (lambda (av)
                               (fv av k))))))]
    
    ;; built-in binary application assumes no unusual transfer of control
    [(_ (f a b))
     #'(lambda (k)
         ((cps a) (lambda (av)
                    ((cps b) (lambda (bv)
                               (k (f av bv)))))))]  
    
    ;; application of lam () - no argument lambda
    [(_ (f))
     #'(lambda (k)
         ((cps f) (lambda (fv)
                    (fv k))))]
        
    [(_ atomic)
     #'(lambda (k) 
         (k atomic))]))

;; c is cps-ed expression
(define (run c) (c identity))


;; let/cc via call/cc
(define-syntax let/cc
  (syntax-rules ()
    [(let/cc k b)
     (call/cc (lambda (k) b))]))


(test (run (cps 3))                           3)
(test (run (cps ((lam ()    5)       )))      5)
(test (run (cps ((lam (x)   (* x x)) 5)))     25)
(test (run (cps (+ 5 ((lam (x) (* x x)) 5)))) 30)

(test (run (cps (+ 1 (let/cc esc (+ 2 (esc 3))))))
      4)

(test (run (cps ((lam (n) 
                 (try/cc throw 
                         (/ 1 (cnd (= n 0)
                                   (throw 0)
                                   n)))) 0)))
      0)

(test (run (cps (with (g (generator (yield) (from)
                              (rec (f (lam (n)
                                           (seq
                                            (yield n)
                                            (f (+ n 1)))))
                                (f from))))
                (+ (g 1) (g 5)))))
      3)
