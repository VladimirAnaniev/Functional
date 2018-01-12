#lang racket

(define (poly-sum x n)
  (define (sumator sum i)
    (cond
      [(> i n) sum]
      [else (sumator (+ sum (expt x i)) (+ i 1))]))
  (sumator 0 0))