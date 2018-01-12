#lang racket

(define (difference f a b)
  (- (f b) (f a)))

(difference (lambda (x) (* x x)) 2 3)