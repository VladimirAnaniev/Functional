#lang racket

; Задача 3. Run-length encoding (RLE) е метод за компресия на списъци, при който
; всяка поредица от еднакви елементи се представя като списък от елемента и
; броя на повторенията му. Да се напише функция encode, която компресира
; даден списък.


(define (encode lst)
  (define (counter xs last count)
    (cond
      [(null? xs) (if (= count 0)
                      '()
                      (cons (cons last count) '()))]
      [(equal? (car xs) last) (counter (cdr xs) last (+ count 1))]
      [else (cons (cons last count) (counter (cdr xs) (car xs) 1))]))
  (counter (cdr lst) (car lst) 1))

(encode '(a a a a b c c a a d e e e e))
(encode '(m i s s i s s i p p i))
