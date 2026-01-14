#lang sicp

; lazy stream constructor
(define (make-stream init compute-next while-condition)
    (let ((current init))
        (define (next)
            (if (null? current)
                `()
                (let ((next-value (compute-next current)))
                    (set! current 
                        (if (while-condition next-value)
                            next-value
                            `())))))
        (define (reset)
            (set! current init))
        (define (is-null?)
            (null? current))
        (define (dispatch m)
            (cond 
                ((eq? m `get-current) current)
                ((eq? m `reset) (reset))
                ((eq? m `next) (next))
                ((eq? m `is-null?) (is-null?))
                (else (error "Undefined method for stream object!" m))))
        dispatch))

; lazy stream selectors
(define (stream-car stream) (stream `get-current))
(define (stream-cdr stream) (begin (stream `next) stream))
(define (stream-null? stream) (stream `is-null?))


; creating lazy enumerated stream
(define (enumerate-interval begin end)
    (make-stream begin (lambda (value) (+ value 1)) (lambda (value) (< value end))))


; functions operating on any type of streams (including lazy and plain lists)
(define (stream-ref stream n)
    (if (or (= n 0) (stream-null? stream))
        (stream-car stream)
        (stream-ref (stream-cdr stream) (- n 1))))

(define (stream-map stream func)
    (if (stream-null? stream)
        `()
        (cons (func (stream-car stream)) (stream-map (stream-cdr stream) func))))

(define (stream-for-each stream func)
    (if (stream-null? stream)
        `done
        (begin 
            (func (stream-car stream))
            (stream-for-each (stream-cdr stream) func))))

(define (display-stream stream)
    (stream-for-each stream (lambda (value) (begin (display value) (newline)))))

(define stream (enumerate-interval 0 10))
(stream-for-each stream (lambda (a) (begin (display a) (newline))))
