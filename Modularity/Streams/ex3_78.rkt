#lang sicp

; Exercise: solve second order diff equation
; d^2y/dt^2 - a*(dy/dt) - by = 0

(define (integral delayed-integrand initial-value dt)
    (define int 
        (stream-cons initial-value
            (let ((integrand (force delayed-integrand)))
                (add-streams (scale-stream integrand dt)
                    int))))
    int)

(define (solve-2d a b dt y0 dy0)
    (define y (integral (delay dy) y0 dt))
    (define dy (integral (delay ddy) dy0 dt))
    (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
    y)
