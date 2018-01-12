#lang racket

(define (minimum xs)
  (apply min xs))


(define (appearances elem xs)
  (length (filter (lambda (x) (= elem x)) xs)))

(define (count-minimum xs)
  (appearances (minimum xs) xs))