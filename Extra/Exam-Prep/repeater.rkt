#lang racket

; Задача 4. Напишете функция (repeater str), която получава като аргумент символен низ и
; връща анонимна функция на два аргумента - count и glue (число и низ). Оценката на
; обръщението към върнатата функция е низ, който се получава чрез count-кратно повтаряне
; на низа str, при което между всеки две съседни повторения на str стои низът glue.

(define (repeater str)
  (define (append-loop glue i max)
    (if (= i max)
        str
        (string-append str glue (append-loop glue (+ i 1) max))))
  (λ (count glue) (append-loop glue 1 count)))

((repeater "I love Racket") 3 " "); -> "I love Racket I love Racket I love Racket"
((repeater "Quack") 5 "!"); -> "Quack!Quack!Quack!Quack!Quack"