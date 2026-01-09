#lang sicp

; Exercise: write function to detect cycles in lists

(define (has-cycle? lst)
    (let ((already-visited `()))
        (define (check-if-visited x visited-pairs)
            (if (null? visited-pairs)
                #f
                (if (eq? x (car visited-pairs))
                    #t
                    (check-if-visited x (cdr visited-pairs)))))
        (define (append visited-pairs x)
            (if (null? visited-pairs)
                (list x)
                (cons (car visited-pairs) (append (cdr visited-pairs) x))))
        (define (append-pair x)
            (set! already-visited (append already-visited x)))
        (define (go-through x)
            (if (not (pair? x))
                #f
                (if (check-if-visited x already-visited)
                    #t
                    (begin
                        (append-pair x)
                        (or (go-through (car x))
                            (go-through (cdr x)))))))
    (if (go-through lst)
        (begin
            (display "List has cycle!")
            (newline))
        (begin
            (display "List doesn't have cycle!")
            (newline)))))


(define no-cycle (cons (cons 1 2) (cons 3 4)))
(define pair (cons (cons 1 2) (cons 3 4)))
(set-cdr! (cdr pair) pair)

(has-cycle? no-cycle)
(has-cycle? pair)