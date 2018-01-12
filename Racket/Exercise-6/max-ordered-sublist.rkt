#lang racket

(define (get-split-index lst i)
  (cond
    [(or (null? lst) (null? (cdr lst))) 1]
    [(>= (car lst) (cadr lst)) i]
    [else (get-split-index (cdr lst) (+ i 1))]))     

(define (split-ordered lst)
  (split-at lst (get-split-index lst 1)))

(define (get-ordered-sublists lst result)
  (if
   (null? lst)
   result
   (let-values ([(current rest) (split-ordered lst)])
     (if (null? rest)
         (cons current result)
         (get-ordered-sublists rest (cons current result))))))
   
(define (max-ordered-sublist lst)
  (foldl
   (lambda (current longest)
     (if (> (length current) (length longest))
         current
         longest))
   '()
   (get-ordered-sublists lst '())))