#lang sicp

(define (pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (iterleave
            (stream-map (lambda (x) (list (stream-car s) (stream-cdr x)))) t)
            (interleave 
                (stream-map (lambda (x) (list x (stream-car s))) (stream-cdr t))
                (pair (stream-cdr s) (stream-cdr t))))))