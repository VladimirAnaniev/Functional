#lang racket


(define (difference f a b)
  (- (f a) (f b)))

(define (derive f eps)
  (lambda (x) (/ (difference f (+ x eps) x) eps)))

(define (derive2 f eps)
  (lambda (x) ((derive (derive f eps) eps) x)))

(define (derive-n f n eps)
  (define (repeat i fp)
    (if
     (> i n)
     fp
     (repeat (+ i 1) (lambda (x) ((derive fp eps) x)))))
  (lambda (x) ((repeat 1 f)x)))
    
