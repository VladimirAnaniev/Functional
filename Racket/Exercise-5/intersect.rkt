#lang racket

(define (contains list elem)
    (let* ([current (car list)]
           [remaining (cdr list)])
      (cond
        [(= current elem) #t]
        [(null? remaining) #f]
        [else (contains remaining elem)])))

(define (intersect set1 set2)
  (define (creator list)
    (cond
      [(null? list) '()]
      [(contains set2 (car list)) (cons (car list) (creator (cdr list)))]
      [else (creator (cdr list))]))

  (creator set1))

(define (intersection set1 set2)
  (filter (lambda (x) (contains set2 x)) set1))