#lang racket

(define (composition f g)
  (lambda (x) (f (g x))))

(define (id x) x)

(define (sum-of-functions functions)
  (lambda (x) (apply + (map (lambda (f) (f x)) functions))))

(define (compose functions result)
  (cond
   [(null? functions) result]
   [(null? (cdr functions)) (cons (composition (car functions) id) result)]
   [else (compose (cddr functions) (cons (composition (car functions) (cadr functions)) result))]))

(define (pair-compose functions)
  (sum-of-functions (compose functions '())))
  