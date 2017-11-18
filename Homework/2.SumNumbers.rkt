#lang racket

; Задача 2. Да се напише функция sum-numbers, която приема един аргумент –
; символен низ и връща сумата на всички числа в него.

(define (sum-numbers str)
  (apply + (filter number? (map string->number (regexp-split #px"\\D" str)))))

(sum-numbers "a123b2c56"); → 181
(sum-numbers "a1b2c3"); → 6