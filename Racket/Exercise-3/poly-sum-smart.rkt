#lang racket

(define (poly-sum x n)
  (define (sumator sum prev pow)
    (cond
      [(> pow n) sum]
      [else (sumator (+ sum (* prev x)) (* prev x) (+ pow 1))]))
  (sumator 1 1 1))