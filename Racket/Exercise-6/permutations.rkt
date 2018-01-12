#lang racket

; Mind-bending
(define (insert-at pos x lst)
  (cond [(null? lst) (list x)]
        [(= pos 0) (cons x lst)]
        [else (cons (car lst) (insert-at (- pos 1) 
                                         x 
                                         (cdr lst)))]))

(define (in-all-positions x lst)
  (map (lambda (pos) (insert-at pos x lst))
       (range (+ 1 (length lst)))))

(define (permutations lst)
  (define (helper res kst)
    (if (null? kst)
        res
        (helper (apply append 
                       (map (lambda (zst)
                             (in-all-positions (car kst) zst))
                     res))
                (cdr kst))))
  (helper '(()) lst))