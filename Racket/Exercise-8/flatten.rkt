#lang racket

(define (flatten lst)
  (apply append (map (λ (x) (if (pair? x) (flatten x) (list x))) lst)))

(flatten '(1 (2 (3 4) 5) (6 7)))