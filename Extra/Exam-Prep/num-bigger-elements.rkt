#lang racket

; Задача 2. Да се напише функция (num-bigger-elements lst), която за даден списък от
; числа lst връща като резултат списък с елементи от вида (lsti ni), където lsti е i-тият
; елемент на lst, а ni е броят на елементите на lst, които са по-големи от lsti.

(define (num-bigger-elements lst)
  (map (λ (x) (list x (length (filter (λ (y) (> y x)) lst)))) lst))

(num-bigger-elements '(5 6 3 4)); → '((5 1) (6 0) (3 3) (4 2))
(num-bigger-elements '(1 1 1)); → '((1 0) (1 0) (1 0))