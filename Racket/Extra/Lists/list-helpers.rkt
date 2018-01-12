#lang racket

(define (minimum xs)
  (apply min xs))

(define (maximum xs)
  (apply max xs))

(define (product xs)
  (apply * xs))
