#lang sicp

; Exercise: limit the number of incorrect password enters


(define (make-account balance password)
    (let ((incorrect-nums 0)
          (acc-is-blocked #f)
          (max-tries 2))
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
                ((eq? action `do-nothing) (lambda (value) (display "")))
                (else (error "Undefined bank account action!"))))
        (define (pass-check pass action)
            (cond
                ((>= incorrect-nums max-tries) 
                    (begin 
                        (set! acc-is-blocked #t)
                        (set! incorrect-nums 0)
                        (display "Too many tries, account has been blocked!")
                        (newline)
                        (dispatch `do-nothing)))
                ((eq? acc-is-blocked #t) (error "Account is blocked!"))
                ((eq? pass password) 
                    (begin 
                        (set! incorrect-nums 0)
                        (dispatch action)))
                (else (begin 
                    (set! incorrect-nums (+ incorrect-nums 1))
                    (display "Incorrect password! Try again.")
                    (newline)
                    (dispatch `do-nothing)))))
        pass-check))

(define acc (make-account 100 `some-pass))
((acc `some-pass `widthdraw) 25) ; prints 75
((acc `some-pass2 `widthdraw) 25) ; incorrect password
((acc `some-pass2 `widthdraw) 25) ; incorrect password
((acc `some-pass2 `widthdraw) 25) ; account has been blocked
((acc `some-pass2 `widthdraw) 25) ; error: account is blocked