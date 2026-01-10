#lang sicp


; This function is used to create bank account object
; This object is using dispatching to get access to
; inner methods
(define (make-account balance)
    (define (widthdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            (error "Not enough money on bank account!")))
    (define (deposit amount)
        (if (> amount 0) 
            (set! balance (+ balance amount))
            (error "The deposit amount should be above zero!")))
    (define (get-balance) balance)
    (define (dispatch action)
        (cond 
            ((eq? action `deposit) deposit)
            ((eq? action `widthdraw) widthdraw)
            ((eq? action `get-balance) get-balance)
            (else (error "Undefined bank account action!"))))
    dispatch)

(define acc1 (make-account 100))
((acc1 `get-balance))
((acc1 `widthdraw) 25) ; prints 75