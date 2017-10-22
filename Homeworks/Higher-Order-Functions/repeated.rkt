#lang racket

(define (repeated f n)
  (define (repeat i acc)
    (if (= i 0)
        acc
        (repeat (- i 1) (f acc))))
  (lambda (x)
    (repeat n x)))

((repeated (lambda (x) (* x 2)) 3) 2) ;f(f(f(2))) = f(f(4)) = f(8) = 16
((repeated (lambda (x) (+ x 1)) 2) 8) ;g(g(8)) = g(9) = 10
