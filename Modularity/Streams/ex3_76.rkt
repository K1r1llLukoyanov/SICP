#lang sicp

(define (smooth input-stream)
    (cons-stream (stream-car input-stream)
        (stream-map 
            (lambda (x y) (/ (+ x y) 2)) 
            input-stream (stream-cdr input-stream))))

(define (make-zero-crossing input-stream)
    (define smoothed (smooth input-stream))
    (steam-map sign-change-detector smoothed (stream-cdr smoothed)))


