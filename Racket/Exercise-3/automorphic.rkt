#lang racket

(define (automorphic? x)
  (define (are-same? a b)
    (cond
      [(= a 0) #t]
      [(= (remainder a 10) (remainder b 10)) (are-same? (quotient a 10) (quotient b 10))]
      [else #f]))
  (are-same? x (* x x)))


(define (asd x y)
  (/ x y))