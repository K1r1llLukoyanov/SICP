#lang sicp

(define (count-pairs x)
    (if (not (pair? x))
        0
        (+  (count-pairs (car x))
            (count-pairs (cdr x))
            1)))

(define lst1 (cons (cons 1 2) (cons 3 4)))
(define lst2 (list (cons 1 2) (cons 3 4)))

(define some-list (list `a `b))
(define lst3 (cons (list `a `b `c) (list `d `e `f)))

(define lst4 (cons (cons 1 2) (cons 3 4)))
(set-cdr! (cdr lst4) lst4)

(count-pairs lst1) ; print 3
(count-pairs lst2) ; print 4
(count-pairs lst3) ; print 7
(count-pairs lst4) ; never ends