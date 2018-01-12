#lang racket

(define (sum-skipping-2 x y acc)
    (if
     (> x y)
     acc
     (sum-skipping-2 (+ 2 x) y (+ acc x))))

(define (sum-odd start end)  
  (if
   (odd? start)
   (sum-skipping-2 start end 0)
   (sum-skipping-2 (+ start 1) end 0)))

(define (sum-even start end)
  (if
   (even? start)
   (sum-skipping-2 start end 0)
   (sum-skipping-2 (+ start 1) end 0)))
