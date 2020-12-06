#lang racket

(require "common.rkt"
         math/number-theory)

(module+ test
  (require rackunit
           syntax/parse/define))

(define (string->digit-list s)
  (filter-map (compose1 string->number ~a)
              (string->list s)))

(define puzzle-input (string->digit-list (file->string "Inputs/input16.txt")))

(define base-pattern '(1 0 -1 0))

(define (step l)
  (for/list ([i (in-range (length l))])
    (remainder
     (abs (for/sum ([d (drop l i)]
                    [p (in-cycle (in-list
                                  (append-map (lambda (v)
                                                (make-list (add1 i) v))
                                              base-pattern)))])
            (* d p)))
     10)))



(define (part1 l)
  (take
   (nth-iterate step l 100)
   8))

(module+ test
  (define-simple-macro (check-first-8-after-100? in ans)
    (check-equal?
     (part1 in)
     ans))
  (test-case "stepping examples"
    (define steps (iterate step '(1 2 3 4 5 6 7 8)
                           4))
    (check-equal? steps '((1 2 3 4 5 6 7 8)
                          (4 8 2 2 6 1 5 8)
                          (3 4 0 4 0 4 3 8)
                          (0 3 4 1 5 5 1 8)
                          (0 1 0 2 9 4 9 8))))
  (test-case "part1 examples"
    (check-first-8-after-100?
     '(8 0 8 7 1 2 2 4 5 8 5 9 1 4 5 4 6 6 1 9 0 8 3 2 1 8 6 4 5 5 9 5)
     '(2 4 1 7 6 1 7 6))
    (check-first-8-after-100?
     '(1 9 6 1 7 8 0 4 2 0 7 2 0 2 2 0 9 1 4 4 9 1 6 0 4 4 1 8 9 9 1 7)
     '(7 3 7 4 5 4 1 8))
    (check-first-8-after-100?
     '(6 9 3 1 7 1 6 3 4 9 2 9 4 8 6 0 6 3 3 5 9 9 5 9 2 4 3 1 9 8 7 3)
     '(5 2 4 3 2 1 3 3)))

  (define have-time? (make-parameter #f))

  (when (have-time?)
    (test-case "my part1 solution"
      (check-first-8-after-100?
       puzzle-input
       '(4 2 9 4 5 1 4 3)))))




(define (base-expansion x p)
  (for/fold ([rem x]
             [coeffs '()]
             #:result (reverse coeffs))
            ([_ (in-range 20)]
             #:break (zero? rem))
    (define taking (remainder rem p))
    (values
     (/ (- rem taking) p)
     (cons taking coeffs))))



(define (binomial-prime m n p #:n-expansion (nfacs #f))
  (for/fold ([ans 1])
            ([mi (in-list (base-expansion m p))]
             [ni (in-list (or nfacs (base-expansion n p)))])
    (modulo (* ans (binomial mi ni)) p)))

(define (binomial-10 m n #:n-expans (nfacs (list #f #f)))
  (solve-chinese (list (binomial-prime m n 5 #:n-expansion (first nfacs))
                       (binomial-prime m n 2 #:n-expansion (second nfacs)))
                 '(5 2)))

(define 99-facs (list (base-expansion 99 5) (base-expansion 99 2)))

(define (part2 l
               (repeat 10000) (pointer-length 7)
               (capture-length 8) (applies 100)
               (nexpans (list #f #f)))
  (define offset (string->number (string-append* (map ~a (take l pointer-length)))))
  (define len (length l))
  (define sub-length (- (* repeat len)
                        offset))
  (define we-care (for/list ([_ (range sub-length)]
                             [x (in-sequences
                                 (in-list (drop l (remainder offset len)))
                                 (in-cycle (in-list l)))])
                    x))
  (define coeffs (build-list
                  sub-length
                  (lambda (j) (binomial-10 (+ (sub1 applies) j) (sub1 applies)
                                           #:n-expans nexpans))))
  (for/list ([i (in-range capture-length)])
    (define line (drop we-care i))
    (for/fold ([tot 0])
              ([el line]
               [coeff coeffs])
      (remainder (+ tot (* el coeff)) 10))))

(module+ test
  (define-simple-macro (check-numstring? f s-in s-exp)
    (check-equal? (string-append* (map ~a (f (string->digit-list s-in))))
                  s-exp))
  (test-case "given part2 examples"
    (check-numstring? part2 "03036732577212944063491565474664"
                      "84462026")
    (check-numstring? part2 "02935109699940807407585447034323"
                      "78725270")
    (check-numstring? part2 "03081770884921959731165446850517"
                      "53553731")))

(define (mod-sums nums)
  (for/fold ([acc 0]
             [coll '()]
             #:result (reverse coll))
            ([n nums])
    (define a (remainder (+ acc n) 10))
    (values a (cons a coll))))

(define (another-part2 l)
  (define offset (string->number (string-append* (map ~a (take l 7)))))
  (define asmuch-backwards (for/list ([_ (range (- (* 10000 (length l))
                                                   offset))]
                                      [x (in-cycle (in-list (reverse l)))])
                             x))
  (take
   (reverse (nth-iterate
             mod-sums asmuch-backwards
             100))
   8))
