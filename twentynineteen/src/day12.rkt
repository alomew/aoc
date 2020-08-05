#lang racket

(require threading)

(struct vec3 (x y z) #:transparent)

(define vec3+
  (match-lambda* [(list (vec3 x y z) (vec3 u v w))
                  (vec3 (+ x u)
                        (+ y v)
                        (+ z w))]))

(struct moon (pos vel) #:transparent)

(define zero-vec3 (vec3 0 0 0))

(define (make-moon x y z)
  (moon (vec3 x y z) zero-vec3))

;; (gravity-pair moon other-moon) produces the effect of other-moon
;; on moon
;; i.e. we return moon but with an affected velocity.
(define gravity-pair
  (match-lambda* [(list (moon (vec3 x y z) vel)
                        (moon (vec3 u v w) _))
                  (define (effect d1 d2)
                    (cond [(= d1 d2) 0]
                          [(< d1 d2) 1]
                          [else -1]))
                  (moon (vec3 x y z)
                        (vec3+ vel
                               (vec3 (effect x u)
                                     (effect y v)
                                     (effect z w))))]))

(define (apply-gravity moons)
  (for/list ([m moons])
    (for/fold ([v-m m])
              ([other-moon moons])
      (gravity-pair v-m other-moon))))

(define (apply-velocity moons)
  (map (match-lambda [(moon pos vel) (moon (vec3+ pos vel)
                                           vel)])
       moons))

(define (time-step moons)
  (~> moons
      apply-gravity
      apply-velocity))

(define (time-steps moons n)
  (for/fold ([moons-now moons])
            ([_ (in-range n)])
    (time-step moons-now)))

(define/match (potential-energy m)
  (((moon (vec3 x y z) _))
   (apply + (map abs (list x y z)))))

(define/match (kinetic-energy m)
  (((moon _ (vec3 dx dy dz)))
   (apply + (map abs (list dx dy dz)))))

(define (energy m) (* (potential-energy m) (kinetic-energy m)))

(define (parse-moons p)
  (for/list ([coord (in-lines p)])
    (match (map string->number (rest (regexp-match #px"^<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>$" coord)))
      [(list x y z)
       (make-moon x y z)])))

(define puzzle-input (call-with-input-file "Inputs/input12.txt" parse-moons))

(define (part1 moons steps)
  (foldl + 0 (map energy (time-steps moons steps))))


(struct flat-moon (u du) #:transparent)

(define/match (flat-gravity-pair fm other-fm)
  (((flat-moon u du) (flat-moon v _))
   (cond [(< u v)
          (flat-moon u (add1 du))]
         [(= u v)
          fm]
         [else
          (flat-moon u (sub1 du))])))

(define (flat-apply-gravity flat-moons)
  (for/list ([f-m flat-moons])
    (for/fold ([now-f-m f-m])
              ([other-f-m flat-moons])
      (flat-gravity-pair now-f-m other-f-m))))

(define (flat-apply-velocity flat-moons)
  (map (match-lambda [(flat-moon u du)
                      (flat-moon (+ u du) du)])
       flat-moons))

(define (flat-time-step flat-moons)
  (~> flat-moons
      flat-apply-gravity
      flat-apply-velocity))

(struct flat-time-stream (state)
  #:methods gen:stream
  ((define (stream-empty? s)
     #f)
   (define (stream-first s)
     (flat-time-stream-state s))
   (define (stream-rest s)
     (flat-time-stream (flat-time-step (flat-time-stream-state s))))))


(define (coords->f-ms coords)
  (map (lambda (u)
         (flat-moon u 0))
       coords))
(define (return-time initial)
  (for/first ([(f-ms i) (in-indexed (in-stream (flat-time-stream initial)))]
              #:unless (= i 0)
              #:when (equal? f-ms initial))
    i))

(define (parse-flat-moons p)
  (apply
   map list
   (map
    (compose1
     coords->f-ms
     (lambda (line)
       (map string->number
            (rest
             (regexp-match #px"^<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>$" line)))))
    (port->lines p))))

(define (part2 p)
  (apply lcm (map return-time (parse-flat-moons p))))

(define (our-part-2)
  (call-with-input-file "Inputs/input12.txt" part2))
