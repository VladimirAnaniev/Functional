#lang racket

(define (bound-up f upper)
  (λ (x) (min (f x) upper)))

(define (bound-down f lower)
  (λ (x) (max (f x) lower)))