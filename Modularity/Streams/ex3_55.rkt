#lang sicp

; Exercise: define procedure partial-sums which takes stream S as argument
; and return stream with elements S0, S0 + S1, S0 + S1 + S2

(define (partial-sums stream)
    (define new-stream
        (cons-stream 
            (stream-car stream) 
            (stream-map + (stream-cdr stream) new-stream)))
    new-stream)

