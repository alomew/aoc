#lang racket

(struct coord (x y) #:transparent)

(define (scaled c)
  (match-define (coord x y) c)
  (define g (gcd x y))
  (coord (/ x g) (/ y g)))

(define (coord- c1 c2)
  (match-define (coord x1 y1) c1)
  (match-define (coord x2 y2) c2)
  (coord (- x1 x2) (- y1 y2)))

(define (coord+ c1 c2)
  (match-define (coord x1 y1) c1)
  (match-define (coord x2 y2) c2)
  (coord (+ x1 x2) (+ y1 y2)))

(define coord-arg
  (match-lambda [(coord x y) (atan y x)]))

(define (clockwise-< c1 c2)
  (define (f t)
    (define y (- t (/ pi 2)))
    (if (< y (- pi))
        (+ y (* 2 pi))
        y))
  (< (f (coord-arg c1)) (f (coord-arg c2))))

(define mag
  (match-lambda [(coord x y) (+ (sqr x) (sqr y))]))

(define (rays source asteroids)
  (for/set ([asteroid asteroids]
            #:unless (equal? source asteroid))
    (scaled (coord- asteroid source))))

(define (part1 asteroids)
  (define (beauty a) (set-count (rays a asteroids)))
  (define most-beautiful (argmax beauty asteroids))
  (values most-beautiful
          (beauty most-beautiful)))

(define (read-asteroid-map m)
  (for*/list ([(row row-n) (in-indexed (in-lines m))]
              [(c col-n) (in-indexed (in-string row))]
              #:when (char=? c #\#))
    (coord col-n row-n)))

(define toy-input
  (read-asteroid-map (open-input-string
                      ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
")))

(define puzzle-input
  (call-with-input-file "Inputs/input10.txt"
    (lambda (in)
      (read-asteroid-map in))))

;; where a ray is the line passing through one
;; or more asteroids, a queue is the list
;; of displacements on one ray, sorted by increasing magnitude
;; recover an asteroid as (coord+ source disp)
(define (queues source asteroids)
  (for/fold ([qs (hash)]
             #:result (sort
                       (hash-map
                        qs
                        (lambda (_ v)
                          (sort v < #:key mag)))
                       clockwise-<
                       #:key first))
            ([ast asteroids]
             #:unless (equal? ast source))
    (hash-update qs (scaled (coord- ast source)) (lambda (ys) (cons (coord- ast source) ys)) empty)))

(define (part2 asteroids source limit)
  (when (= limit 0)
    (error 'bag-args))
  (define (helper live-queues left)
    (if (<= left (length live-queues))
        (coord+ (first (list-ref live-queues (sub1 left)))
                source)
        (helper (filter-not empty?
                            (map rest live-queues))
                (- left (length live-queues)))))
  (helper (queues source asteroids) limit))
