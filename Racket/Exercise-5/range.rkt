#lang racket

(define (rng a b)
  (define (creator start current list)
    (cond
      [(= current start) list]
      [else (creator start (- current 1) (cons current list))]))
  (creator a b '()))

(define (range a b)
  (if
    (= a b)
    '()
    (cons a (range (+ a 1) b))))
    