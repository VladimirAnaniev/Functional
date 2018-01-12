#lang racket

(define (replace lst dict)
  (define (replace elem)
    (let ([found (findf (lambda (x) (= (car x) elem)) dict)]) 
    (if (false? found)
        elem
        (cdr found))))
      
  (map replace lst))