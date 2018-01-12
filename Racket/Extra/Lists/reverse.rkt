#lang racket

(define (reverse ls)
  (define (rev-helper reversed remaining)
    (if
      (null? remaining)
      reversed
      (rev-helper (cons (car remaining) reversed) (cdr remaining))))
  (rev-helper '() ls))