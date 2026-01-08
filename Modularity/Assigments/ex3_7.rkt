#lang sicp

; Exercise: create Make-joint function
; This function enables creating of split account
; This function should get 3 arguments
; 1. Account object with password
; 2. Password for this account
; 3. New password for new account
; New account should access account object from first argument



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


(define (Make-joint split-account password new-password)
    (lambda (pass action)
        (if (eq? pass new-password) 
            (split-account password action)
            (error "Password is wrong!"))))

(define acc (make-account 100 `some-pass))
(define split-acc (Make-joint acc `some-pass `split-pass))
((acc `some-pass `widthdraw) 25) ; prints 75
((split-acc `split-pass `widthdraw) 25) ; print 50
((acc `some-pass `widthdraw) 25) ; prints 25