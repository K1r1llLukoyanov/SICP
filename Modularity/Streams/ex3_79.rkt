#lang sicp

; Exercise: modify previos exercise for general usage

(define (solve-2d f y0 dy0 dt)
    (define y (integral (delay dy) y0 dt))
    (define dy (integral (delay ddy) dy0 dt))
    (define ddy (map-stream f dy y))
    y)

