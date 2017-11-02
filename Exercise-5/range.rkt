#lang racket

(define (range a b)
  (define (creator start current list)
    (cond
      [(< current start) list]
      [else (creator start (- current 1) (cons current list))]))
  (creator a b '()))