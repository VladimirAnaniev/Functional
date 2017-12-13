#lang racket

(define (tree root left right)
  (cons root (list left right)))

(define example1 (tree 5 (tree 2 1 3) (tree 8 7 9)))
(define example2 (tree 5 (tree 2 (tree 1 0 2) 3) (tree 8 7 (tree 10 9 11))))

(define (root tree)
  (car tree))

(define (left tree)
  (cadr tree))

(define (right tree)
  (caddr tree))

(define (nodes-count tree)
  (cond
    [(null? tree) 0]
    [(number? tree) 1]
    [else (+ 1 (nodes-count (left tree)) (nodes-count (right tree)))]))

(define (sum-nodes tree)
  (cond
    [(null? tree) 0]
    [(number? tree) tree]
    [else (+ (root tree) (sum-nodes (left tree)) (sum-nodes (right tree)))]))

(define (add t node)
  (cond
    [(null? t) (tree node '() '())]
    [(number? t) (if (> t node) (tree t node '()) (tree t '() node))]
    [(> (root t) node) (tree (root t) (add (left t) node) (right t))]
    [else (tree (root t) (left t) (add (right t) node))]))

(define (left-root-right tree)
  (cond
    [(null? tree) '()]
    [(number? tree) (list tree)]
    [else (append (left-root-right (left tree)) (list (root tree)) (left-root-right (right tree)))]))

(define (sort-with-tree lst)
  (left-root-right (foldl (λ (curr acc) (add acc curr)) '() lst)))

(define (path t node)
  (cond
    [(null? t) (error "Node not in the tree")]
    [(number? t) (path (tree t '() '()) node)]
    [(= (root t) node) (cons node '())]
    [(> (root t) node) (cons (root t) (path (left t) node))]
    [else (cons (root t) (path (right t) node))]))

(define (path-between t n1 n2)
  (define (remove-similar path1 path2)
    (if (= (car path1) (car path2))
        (if (= (cadr path1) (cadr path2))
            (remove-similar (cdr path1) (cdr path2))
            (cons path1 (cdr path2)))
        (cons (path1) (path2))))
  (let* ([paths (remove-similar (path t n1) (path t n2))]
         [p1 (car paths)]
         [p2 (cdr paths)])
    (append (reverse p1) p2)))

