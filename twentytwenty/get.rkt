#! /usr/bin/env racket

#lang racket

(require gregor)

(define session (string-trim (file->string "cookie.secret")))

(define (get-date d)
  (define outfile (format "day~a.input" (->day d)))
  (define url (format "https://adventofcode.com/~a/day/~a/input" (->year d) (->day d)))
  (unless (file-exists? outfile)
    (system* (find-executable-path "curl")
             "-b" (format "session=~a" session)
             "-o" outfile
             url)))

(define t (today #:tz "America/New_York"))

(define advent (for/list ([d (in-range 1 26)])
                 (date 2020 12 d)))

(define (get)
  (for ([day (in-list advent)]
        #:when (date<=? day t))
    (get-date day)))

(module+ main
 (get))
