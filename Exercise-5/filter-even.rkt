#lang racket

(define (filter-even list)
  (filter even? list))

(define (my-filter-even list)
  (define (creator list res)
    (let* ([curr (car list)]
           [rest (cdr list)])
      (cond
        [(null? rest) (if (even? curr) (cons curr res) res)]
        [(even? curr) (creator rest (cons curr res))]
        [else (creator rest res)])))
  (creator list '()))