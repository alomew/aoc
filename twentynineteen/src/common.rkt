#lang racket

(provide iterate
         nth-iterate)

(define (iterate f x n)
  (for/fold ([xs (list x)] [prev x] #:result (reverse xs))
            ([_ (in-range n)])
    (define next (f prev))
    (values (cons next xs) next)))

(define (nth-iterate f x n)
  (for/fold ([ans x])
            ([_ (in-range n)])
    (f ans)))
