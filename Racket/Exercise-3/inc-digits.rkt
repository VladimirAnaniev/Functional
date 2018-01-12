#lang racket

(define (inc-digits? x)
  (define (check-digits num a b)
    (cond
      [(= num 0) (< a b)]
      [(> a b) #f]
      [else (check-digits (quotient num 10) (remainder num 10) a)]))
  (if (= 0 (quotient x 10))
      #t
      (check-digits (quotient x 100) (remainder (quotient x 10) 10) (remainder x 10))))