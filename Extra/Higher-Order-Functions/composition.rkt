#lang racket

(define (composition f g)
  (lambda (x) (f (g x))))

((composition (lambda (x) (* 2 x)) (lambda (y) (+ y 1))) 2)
