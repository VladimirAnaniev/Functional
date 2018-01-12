#lang racket

(define (palindromes-count start end)
  (define (palindrome? n)
    (define (reverse x sum)
      (cond
        [(= x 0) sum]
        [else (reverse (truncate (/ x 10)) (+ (* sum 10) (remainder x 10)))]))
    (= n (reverse n 0)))
  (define (count sum i)
    (cond
      [(= i end) sum]
      [(palindrome? i) (count (+ sum 1) (+ i 1))]
      [else (count sum (+ i 1))]))
  (count 0 start))