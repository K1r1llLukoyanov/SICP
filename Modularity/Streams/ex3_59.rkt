#lang sicp

; Exercise: define integrate-series which gets
; stream (a0, a1, a2, ...) and returns 
; stream (a0, 1/2 * a1, 1/3 * a2)

(define (integrate-series coeffs)
    (define (iter stream series-num)
        (cons-stream 
            (/ (stream-car stream) series-num)
            (iter (stream-cdr stream) (+ series-num 1))))
    (iter coeffs 1))

(define exp-series
    (cons-stream 1 (integrate-series exp-series)))

(define cosine-stream
    (cons-stream 1 (integrate-series (stream-map - sine-stream))))

(define sine-stream
    (cons-stream 0 (integrate-series cosine-stream)))