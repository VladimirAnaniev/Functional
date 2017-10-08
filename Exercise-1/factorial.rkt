#lang racket

(define (fact x)
  (if (= x 1)
      1
      (* x (fact (- x 1)))))

(define (factorial x)
  (define (factorial-rec acc i)
    (if (= i x)
        (* acc i)
        (factorial-rec (* acc i) (+ i 1))))
  (factorial-rec 1 1))