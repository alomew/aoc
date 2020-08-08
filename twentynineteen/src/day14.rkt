#lang racket

(module+ test
  (require rackunit))

(struct ingredient (substance quantity) #:transparent)

(struct recipe (substance quantity ingredients) #:transparent)

(struct base-react (ore-ing other-ing) #:transparent)

(define (read-ingredient c)
  (match-define (list _ q s)
    (regexp-match #px"(\\d+) ([[:upper:]]+)" c))
  (ingredient s (string->number q)))

(define (read-manifest m)
  (for/hash ([l (in-lines m)])
    (match-define (list ingredients result) (string-split l " => "))
    (match-define (ingredient s q) (read-ingredient result))
    (values s (recipe s q (map read-ingredient (string-split ingredients ", "))))))

(define toy-data (read-manifest (open-input-string "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 2 D
7 A, 2 D => 1 E
7 A, 1 E => 1 FUEL")))

(define toy-data-2 (read-manifest (open-input-string "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")))

(define toy-data-3 (read-manifest (open-input-string "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF")))

(define toy-data-4 (read-manifest (open-input-string "171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX")))

(define puzzle-input (call-with-input-file "Inputs/input14.txt" read-manifest))

(define (zero-congs s l k)
  (+ (quotient l k)
     (if (and (positive? (remainder l k))
              (or
               (zero? (remainder s k))
               (> (remainder l k) (- k (remainder s k)))))
         1
         0)))

(module+ test
  (test-case "zero-congs on various inputs"
    (check-eq? (zero-congs 0 3 4) 1)
    (check-eq? (zero-congs 0 4 4) 1)
    (check-eq? (zero-congs 3 4 4) 1)
    (check-eq? (zero-congs 2 2 4) 0)
    (check-eq? (zero-congs 1 11 4) 2)
    (check-eq? (zero-congs 2 11 4) 3)))

(define (traverse-deps d fuel-quantity)
  (define recipe-reqs (make-hash))
  (define (request-recipe! rec q)
    (match-define (recipe s sq ings) rec)
    (let* ([old-r (hash-ref recipe-reqs rec 0)]
           [new-r (+ old-r q)]
           [ticks (zero-congs old-r q sq)])
      (hash-set! recipe-reqs rec new-r)
      (when (positive? ticks)
        (for ([ing ings])
          (match-define (ingredient ing-s ing-q) ing)
          (when (not (string=? ing-s "ORE"))
            (request-recipe! (hash-ref d ing-s) (* ticks ing-q)))))))
  (request-recipe! (hash-ref d "FUEL") fuel-quantity)
  recipe-reqs)

(define/match (ore-needed rec quant)
  [((recipe _ s-quant (list (ingredient "ORE" ore-quant))) _)
   (* (ceiling (/ quant s-quant)) ore-quant)]
  [(_ _) 0])

(define (ore-required d fuel-quantity)
  (for/sum ([p (in-dict-pairs (traverse-deps d fuel-quantity))])
    (match-define (cons rec q) p)
    (ore-needed rec q)))

(define (part1 d)
  (ore-required d 1))

(module+ test
  (test-case "part1 examples"
    (check-equal? (part1 toy-data) 31)
    (check-equal? (part1 toy-data-2) 13312)
    (check-equal? (part1 toy-data-3) 180697)
    (check-equal? (part1 toy-data-4) 2210736))
  (test-case "part1 my puzzle input"
    (check-eq? (part1 puzzle-input) 892207)))


(define (fuel-search d ore-limit)
  (define (helper low high)
    (if (= low high)
        low
        (let* ([mid (ceiling (/ (+ low high) 2))]
               [ore-req (ore-required d mid)])
          (cond
            [(= ore-limit ore-req)
             mid]
            [(< ore-req ore-limit)
             (helper mid high)]
            [else
             (helper low (sub1 mid))]))))
  (helper 0 ore-limit))

(define (part2 d)
  (fuel-search d (expt 10 12)))

(module+ test
  (test-case "part2 examples"
    (check-equal? (part2 toy-data-2) 82892753)
    (check-equal? (part2 toy-data-3) 5586022)
    (check-equal? (part2 toy-data-4) 460664))
  (test-case "part1 my puzzle input"
    (check-eq? (part2 puzzle-input) 1935265)))
