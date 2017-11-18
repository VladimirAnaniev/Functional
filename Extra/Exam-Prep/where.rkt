#lang racket

; Задача 8. Да се напише функция (where list-elements list-predicates), която
; връща списък от всички елементи на list-elements, за които са изпълнени всички
; предикати в list-predicates.

(define (where elements predicates)
  (filter (λ (element) (foldl (λ (predicate result) (and (predicate element) result)) #t predicates)) elements))

(where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))); → (6 8 10) (списък от всички елементи на дадения, които са четни числа, по-големи от 5)
(where '(3 4 5 7) (list even? (lambda (x) (> x 5)))); → () (в списъка няма четни числа, по-големи от 5)