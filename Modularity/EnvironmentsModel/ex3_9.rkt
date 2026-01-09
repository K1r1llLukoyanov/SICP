#lang sicp

; Exercise: describe call to (factorial-iter 6)
; and (factorial-rec 6)

(define (factorial-rec n)
    (if (= n 1)
        1
        (* n (factorial-rec (- n 1)))))

(define (factorial-iter a)
    (fact-iter 1 1 a))

(define (fact-iter counter product n)
    (if (> counter n)
        product
        (fact-iter (+ counter 1) (* product counter) n)))


; recursive function call
(factorial-rec 6)
; this call creates environment with binding n = 6
; if: n == 1 we return 1
; else: we call (factorial-rec 5). This call create
; another environment with n = 5.
; This process continues until no new environments
; are being created.
; So for evaluating (factorial-rec 6) 6 environments
; are created

; iterative function call
(factorial-iter 6)
; This call create environment with binding a = 6 
; This function call function (fact-iter 1 1 6)
; so another environment is created with bindings
;   counter: 1
;   product: 1
;   n: 6
; then if counter > 6: we return product
; else: we call (fact-iter 2 1 6) and another environment
; is created with bindings
;   counter: 2
;   product: 1
;   n: 6
; This process continues until no new environments are being
; created

; So during call to (factorial-iter 6) 7 environments were
; created


