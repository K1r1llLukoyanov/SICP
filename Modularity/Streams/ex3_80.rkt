#lang sicp

; Exercise: write procedure RLC which simulates sequential
; RLC-circuit and return two streams: v_C and i_L

(define (sub-streams s1 s2)
    (map-streams - s1 s2))

(define (RLC R L C dt)
    (lambda (v_C0 i_L0)
        (define v_C (intergal (delay (scale-stream i_L (/ 1.0 C))) v_C0 dt))
        (define i_L (integral (delay
            (sub-streams 
                (scale-stream v_C (/ 1.0 L))
                (scale-stream i_L (/ R L)))) i_L0 dt))
        (cons v_C i_L)))

