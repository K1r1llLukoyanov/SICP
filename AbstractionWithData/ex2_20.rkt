#! /usr/bin/env racket
#lang racket

(define (same-parity a . b)
    (define (same-parity-helper lst parity-lst)
        (if (null? lst) parity-lst
            (if (= (remainder a 2) (remainder (car lst) 2))
                (same-parity-helper (cdr lst) (append parity-lst (list (car lst))))
                (same-parity-helper (cdr lst) parity-lst)
            )))
    (same-parity-helper (append (list a) b) (list)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
