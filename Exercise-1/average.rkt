#lang racket

(define (avg a b)
  (/ (+ a b) 2))

(define (average . args)
  (/
   (apply + args)
   (length args)))

