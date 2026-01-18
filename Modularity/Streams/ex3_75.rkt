#lang sicp

; Exercise: Make better version of zero-crossing function


(define (make-zero-crossing input-stream last-value)
    (let (
        (avpt1 (/ (+ (stream-car input-stream) last-value) 2))
        (avpt2 (/ (+ avpt1 (stream-car intput-stream)) 2)))
        (cons-stream (sign-change-director avpt1 avpt2)
            (make-zero-crossing (stream-cdr input-stream) avpt1))))



