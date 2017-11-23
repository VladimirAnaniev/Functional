#lang racket

(define (reduce-mat mtx)
  (map (λ (x) (cdr x)) (cdr mtx)))

(define (get-diagonal mtx)
  (cond
    [(null? mtx) '()]
    [else (cons (caar mtx) (get-diagonal (reduce-mat mtx)))]))

(define (rotate mtx)
  (foldl
    (λ (x acc)
         (cond
           [(null? acc) (append (map (λ (y) (cons y '())) x) acc)]
           [else (map cons x acc)]))
    '() mtx))


(define (diagonal-product mtx)
  (foldl (λ (x y acc) (+ acc (* x y))) 0 (get-diagonal mtx) (reverse (get-diagonal (rotate mtx)))))

(diagonal-product '((1 2 3) (4 5 6) (7 8 9)))