#lang sicp

; Exercise: Write corrent version of count-pairs
; add list for visited pairs

(define (count-pairs x)
    (let ((already-visited `()))
        (define (check-if-visited x visited-lst)
            (if (null? visited-lst)
                #f
                (if (eq? (car visited-lst) x)
                    #t
                    (check-if-visited x (cdr visited-lst)))))
        (define (append x y)
            (if (null? x)
                (list y)
                (cons (car x) (append (cdr x) y))))
        (define (append-visited x)
            (set! already-visited (append already-visited x)))
        (define (count-pair x)
            (if (not (pair? x))
                0
                (if (check-if-visited x already-visited)
                    0
                    (begin 
                        (append-visited x)
                        (+  (count-pair (car x))
                            (count-pair (cdr x))
                            1)))))
        (count-pair x)))


(define p (cons 1 2))
(define p1 (cons p p))
(define p2 (cons (cons 1 2) (cons 3 4)))
(define p3 (cons p1 p1))
(define p4 (cons (cons 1 2) (cons 3 4)))
(set-cdr! (cdr p4) p4)

(count-pairs p1) ; prints 2
(count-pairs p2) ; prints 3
(count-pairs p3) ; prints 3
(count-pairs p4) ; prints 4
