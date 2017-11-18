#lang racket

; Задача 1. Да се напише функция (convert x k n), която получава цяло число x ≥ 0,
; записано в k-ична позиционна бройна система, и връща съответното му число в
; n-ична бройна система, където 2 ≤ k, n ≤ 10.
; Забележка. Не е позволена употребата на символни низове, както и на всякакви
; вградени функции, които получават директно резултата.

(define (to-base-10 num base)
  (cond
    [(zero? (quotient num 10)) num]
    [else (+ (remainder num 10) (* base (to-base-10 (quotient num 10) base)))]))


(define (to-base-n num base)
  (cond
    [(zero? (quotient num base)) num]
    [else (+ (remainder num base) (* 10 (to-base-n (quotient num base) base)))]))

(define (convert x start end)
  (to-base-n (to-base-10 x start) end))

(convert 123 10 2); → 1111011
(convert 173 8 10); → 123