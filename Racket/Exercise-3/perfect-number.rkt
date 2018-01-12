#lang racket

(define (perfect? x)
  (define (sumator sum i end)
    (cond
      [(= i end) sum]
      [(= 0 (remainder x i)) (sumator (+ sum i) (+ i 1) end)]
      [else (sumator sum (+ i 1) end)]))
  (= x (sumator 0 1 x)))

  