#lang sicp

; Exercise: create effective method to generate stream of numbers
; whose divisor are only 2, 3 and 5

(define (merge-streams stream1 stream2)
    (cond 
        ((stream-null? stream1) stream2)
        ((stream-null? stream2) stream1)
        (else
            (cond ((eq? (stream-car stream1) (stream-car stream2))
                    (cons-stream 
                        (stream-car stream1)
                        (merge-streams (stream-cdr stream1) (stream-cdr stream2))))
                ((< (stream-car stream1) (stream-car stream2))
                    (cons-stream
                        (stream-car stream1)
                        (merge-streams (stream-cdr stream1) stream2)))
                (else
                    (cons-stream
                        (stream-car stream2)
                        (merge-streams stream1 (stream-cdr stream2))))
            ))))

(define hamming-stream
    (cons-stream 1
        (merge-streams
            (merge-streams  
                (scaled-stream hamming-stream 2) 
                (scaled-stream hamming-stream 3)) 
            (scaled-stream hamming-stream 5))))

