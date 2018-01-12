#lang racket

(define (transpose mtx)
   (foldl
    (λ (x acc)
         (cond
           [(null? acc) (append x acc)]
           [else (map list acc x)]))
    '() mtx))

(transpose '((1 2 3 4) (4 5 6 7) (7 8 9 10) (11 12 13 14)))