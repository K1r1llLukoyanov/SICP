#lang sicp

; Exercise: Create imperative function that depends on
; operations order

(define (make-closure)
    (let ((prev-value (- 1)))
        (define (inner value)
            (if (= prev-value 0)
                (begin (set! prev-value value) 0)
                (begin (set! prev-value value) value)))
    inner))


(define f (make-closure))
(+ (f 0) (f 1)) ; left to right
(+ (f 1) (f 0)) ; right to left