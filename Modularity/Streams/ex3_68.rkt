#lang sicp

; Exercise: estimate Hugo decision

(define (pairs s t)
    (interleave
        (stream-map (lambda (x) (list (stream-car s)) t))
        (pairs (stream-cdr s) (stream-cdr t))))

; before calling to interleave
; interpreter calculating it's arguments
; so procedure pairs calls itself and we get infinite recursion