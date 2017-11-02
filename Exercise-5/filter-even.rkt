#lang racket

(define (filter-even list)
  (filter even? list))

(define (my-filter-even list)
  (define (creator list)
      (cond
        [(null? list) '()]
        [(even? (car list)) (cons (car list) (creator (cdr list)))]
        [else (creator (cdr list))]))
  (creator list))