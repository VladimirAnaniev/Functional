#lang racket

(define (num-bigger-elements list)
  (define (count-bigger xs elem count)
    (cond
      [(null? xs) count]
      [(> (car xs) elem) (count-bigger (cdr xs) elem (+ count 1))]
      [else (count-bigger (cdr xs) elem count)]))

  (define (creator xs)
    (cond
      [(null? xs) '()]
      [else
       (let* ([current (car xs)]
              [bigger (count-bigger list (car xs) 0)])
         (cons (cons current bigger) (creator (cdr xs))))]))

  (creator list))