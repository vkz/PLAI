#lang plai-typed

(define-syntax-rule (let/cc k b)
  (call/cc (lambda (k) b)))

(define d display) ;; a useful shorthand in what follows
 


(define-type ThreadStatus
  [Tsuspended]
  [Tdone])


(define (scheduler-loop-1 threads)
  (cond
    [(empty? threads) 'done]
    [(cons? threads)
     (type-case ThreadStatus (let/cc after-thread ((first threads) after-thread))
       [Tsuspended () (scheduler-loop-1 (append (rest threads)
                                                (list (first threads))))]
       [Tdone () (scheduler-loop-1 (rest threads))])]))

(define-syntax thread-0
  (syntax-rules ()
    [(thread (yielder) b ...)
     (letrec ([thread-resumer (lambda (_)
                                (begin b ...
                                       (finisher)))]
              [finisher (lambda () (error 'finisher "nothing here"))]
              [yielder (lambda () (error 'yielder "nothing here"))])
       (lambda (sched-k)
         (begin
           (set! finisher         
                 (lambda () (sched-k (Tdone))))
           (set! yielder
                 (lambda ()
                         (let/cc thread-k
                           (begin
                             (set! thread-resumer thread-k)
                             (sched-k (Tsuspended))))))
           (thread-resumer 'tres))))]))

(scheduler-loop-1
 (list
  (thread-0 (y) (d "t1-1  ") (y) (d "t1-2  ") (y) (d "t1-3 "))
  (thread-0 (y) (d "t2-1  ") (y) (d "t2-2  ") (y) (d "t2-3 "))
  (thread-0 (y) (d "t3-1  ") (y) (d "t3-2  ") (y) (d "t3-3 "))))