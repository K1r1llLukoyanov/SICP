#lang sicp

; Exercise: create password protected bank-account objects


(define (make-account balance password)
    (define (widthdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            (error "Not enough money on bank account!")))
    (define (deposit amount)
        (if (> amount 0) 
            (set! balance (+ balance amount))
            (error "The deposit amount should be above zero!")))
    (define (get-balance) balance)
    (define (dispatch pass action)
        (if (not (eq? pass password))
            (error "Password is incorrect!")
            (cond 
                ((eq? action `deposit) deposit)
                ((eq? action `widthdraw) widthdraw)
                ((eq? action `get-balance) get-balance)
                (else (error "Undefined bank account action!")))))
    dispatch)

(define acc (make-account 100 `some-password))
((acc `some-password `widthdraw) 25) ; prints 75
((acc `some-other-password `widthdraw) 25) ; error "Password is incorrect!"