#lang racket

(define-syntax-rule (generator (yield) (v) b)
  (let ([where-to-yield (lambda (v) (error 'dummy "should never get here"))])
    (letrec ([resumer (lambda (v) 
                        (begin
                          b
                          (error 'generator "fell through")))]
             
             [yield (lambda (v)
                      (let/cc gen-k
                        (begin
                          (set! resumer gen-k)
                          (where-to-yield v))))])      
      (lambda (v) 
        (let/cc dyn-k
          (begin
            (set! where-to-yield dyn-k)
            (resumer v)))))))



(let ([g (generator (yield) (v) 
                          (letrec ([loop (lambda (n)
                                       (begin
                                         (yield n)
                                         (loop (+ n 1))))])
                            (loop v)))])
        (+ (g 1) (g 5)))