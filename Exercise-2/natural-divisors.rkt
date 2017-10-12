#lang racket

(define (divisors-count n)
  (define (count sum i)
    (cond
      [(= i n) sum]
      [(= 0 (modulo n i)) (count (+ sum 1) (+ i 1))]
      [else (count sum (+ i 1))]))
  (count 0 1))