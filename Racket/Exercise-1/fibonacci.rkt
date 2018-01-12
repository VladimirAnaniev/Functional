#lang racket

(define (fib-bad x)
  (cond
    [(= x 0) 1]
    [(= x 1) 1]
    [else (+ (fib (- x 1)) (fib (- x 2)))]))

(define (fibonacci n)
  (define (fibonacci-help first second remaining)
    (if (= remaining 0) 
        (+ first second)
        (fibonacci-help second (+ first second) (- remaining 1))))
  (cond
    [(= n 0) 1]
    [(= n 1) 1]
    [else (fibonacci-help 1 1 (- n 2))]))



(define (fib n)
  (define (fib-rec first second i)
    (if (= i n)
        (+ first second)
        (fib-rec second (+ first second) (+ i 1))))
  (if (< n 2)
      1
      (fib-rec 1 1 2)))