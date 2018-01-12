#lang racket

; Задача 1. Да се напише функция (sum-numbers a b), приемаща два аргумента, която
; намира сумата на числата в интервала [a,b], чиито цифри са в низходящ (>=) ред.

(define (generate-list a b)
  (if (> a b)
      '()
      (cons a (generate-list (+ a 1) b))))

(define (descending? num)
  (let* ([last (remainder num 10)]
         [pre-last (remainder (quotient num 10) 10)])
    (cond
      [(< num 10) #t]
      [(>= pre-last last) (descending? (quotient num 10))]
      [else #f])))

(define (sum-numbers a b)
  (apply + (filter descending? (generate-list a b))))

(sum-numbers 1 9); → 45
(sum-numbers 199 203); → 200
(sum-numbers 219 225); → 663