#lang racket

(define (has-n-leading-zeros? list n)
  (cond
    [(zero? n) #t]
    [(not (zero? (car list))) #f]
    [else (has-n-leading-zeros? (cdr list) (- n 1))]))

(define (triangular? matrix)
  (define (checker mat i)
    (cond
      [(null? mat) #t]
      [(has-n-leading-zeros? (car mat) i) (checker (cdr mat) (+ i 1))]
      [else #f]))
  (checker matrix 0))
  