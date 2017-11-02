#lang racket

(define (intersect set1 set2)
  (define (contains list elem)
    (let* ([current (car list)]
           [remaining (cdr list)])
      (cond
        [(= current elem) #t]
        [(null? remaining) #f]
        [else (contains remaining elem)])))

  (define (creator list result)
    (let* ([current (car list)]
           [remaining (cdr list)])
      (cond
        [(contains set2 current)
         (if
          (null? remaining)
          (cons current result)
          (creator remaining (cons current result)))]
        [(null? remaining) result]
        [else (creator remaining result)])))

  (creator set1 '()))