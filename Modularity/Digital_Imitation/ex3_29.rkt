#lang sicp

; Exercise: define or-gate using and-gate and inverter

(define (or-gate a b c)
    (define (add-action-procedure)
        (let 
            ((d (make-wire))
            (e (make-wire))
            (f (make-wire)))
            (inverter a d)
            (inverter b e)
            (and-gate d e f)
            (inverter f c)))
    (add-action! a add-action-procedure)
    (add-action! b add-action-procedure)
    `ok)

; or-gate delay is equal to inverted-delay*2 + and-gate-delay