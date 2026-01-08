#lang sicp

(define global-balance 100)

; This function is setting new balance value of global balance
; Easy to use, but global balance could be modified from any point of
; program, hard to maintain and debug. Pure security
(define (global-widthdraw amount)
    (if (<= amount global-balance) 
        (set! global-balance (- global-balance amount)) 
        (error "Not enough money on global balance")))

; After calling this function we return anonymous
; function which create closure and saves state between calls
; so now all data is saved in this function and can be modified
; only from inside, no global access to the data
(define (make-simplified-widthdraw balance)
    (lambda (amount)
        (set! balance (- balance amount))
        balance))

(display global-balance) ; prints 100
(newline)

(global-widthdraw 25)
(display global-balance) ; prints 75
(newline)

; W1 is not pointing to anonymous function
; Call to W1 modifies its state
(define W1 (make-simplified-widthdraw 100))
(display (W1 25)) ; prints 75
(newline)
(display (W1 25)) ; prints 50
(newline)


; Assignments break substitution model of computations
; Substitution only works if objects are equivalent
; so they are "the same", always have the equivalent state
; and behavior. 

; If two objects can have different states they can't be
; substituted. Actions on objects that cause changes of
; states are called 'side effects'

; Paradigm of programming that obuse power of assigments
; is called imperative programming


; Example of differents between functional and imperative
; programming


; Factorial(Functional programming)
(define (factorial-funct n)
    (define (iter counter result) 
        (if (> counter n)
            result
            (iter (+ counter 1) (* result counter))))
    (iter 1 1))


; Factorial (imperative programming)
(define (factorial-imper n)
    (let ((product 1)   ; definions of variables
          (counter 1))
        (define (iter)
            (if (> counter n)
                product
                (begin 
                    (set! product (* product counter))  ; changing states
                    (set! counter (+ counter 1))        ; of variables
                    (iter))))
    (iter)))


