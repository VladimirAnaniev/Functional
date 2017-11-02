#lang racket

(define (intersect set1 set2)
  (define (contains list elem)
    (let* ([current (car list)]
           [remaining (cdr list)])
      (cond
        [(= current elem) #t]
        [(null? remaining) #f]
        [else (contains remaining elem)])))

  (define (creator list)
    (cond
      [(null? list) '()]
      [(contains set2 (car list)) (cons (car list) (creator (cdr list)))]
      [else (creator (cdr list))]))

  (creator set1))