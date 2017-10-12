#lang racket

(define (prime? x)
  (define (is-prime-rec iter end)
    (cond
      [(> iter end) #t]
      [(= 0 (modulo x iter)) #f]
      [#t (is-prime-rec (+ 1 iter) end)]))
  (if (< x 2) #f (is-prime-rec 2 (sqrt x))))

(define (sum-primes n k)
  (define (sum-primes-iter x i sum)
    (cond
      [(= i 0) sum]
      [(prime? x) (sum-primes-iter (+ 1 x) (- i 1) (+ x sum))]
      [else (sum-primes-iter (+ 1 x) i sum)]))
  (sum-primes-iter (+ 1 k) n 0))
                  
  
