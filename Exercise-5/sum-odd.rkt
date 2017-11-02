#lang racket

(define (sum-odd list)
  (define (sum acc list)
    (if
     (null? list)
     acc
     (sum (+ acc (car list)) (cdr list))))
  (sum 0 (filter odd? list)))