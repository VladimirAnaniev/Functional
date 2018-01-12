#lang racket

(define (sum-uniq xs)
  (apply + (filter (lambda (x) (not (member x (cdr (member x xs))))) xs))) 

(define (sum-unique xss)
  (apply + (map (lambda (xs) (sum-uniq xs)) xss)))