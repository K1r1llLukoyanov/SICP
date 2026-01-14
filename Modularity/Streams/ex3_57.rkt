#lang sicp

; Exercise: explain how memo_proc reduces number of operations
; in generating fibs

(define (memo-proc proc)
    (let ((already-run? false) (result false))
        (lambda ()
            (if (not already-run?)
                (begin (set! result (proc))
                (set! already-run? true)
                result)
                result))))

(define (stream-map proc . argstreams)
  (if (any stream-null? argstreams)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (memo-proc (lambda () b))))))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define fibs (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

; for first 2 terms of fibs no arithmetics is needed, they are hardcoded
; for 3rd term (add-streams is called)

; add-streams procedure uses stream-map with arguments
; (stream-cdr fibs) and fibs

; when (stream-cdr fibs) was called memo-proc was called for first time
; it sets already-run? to true and returns fibs stream starting from 1
; add-streams returns new stream starting with sum of prev two numbers
; of fib stream

; fib stream starts with (F0, F1, ...)
; so (car fib) = F0
; and (cdr fib) = F1 (calls to memo-proc to save function result)

; F2 = F0 + F1 (calling memo-proc to get F1 value)
; F3 = F2 + F1 (here we need to call to memo-proc again to get F1, 
;               but result is already saved and we only need to use one addition)

; F2 = F0 + F1 (1 op)
; F3 = F2 + F1 = F0 + F1 + F1 (2 ops needed)
; F4 = F3 + F2 (1op + 2op + 1op = 4 op)
; F5 = F4 + F3 = (2op + 4 op + 1 op = 7 op)

; Fn = F(n-1) + F(n-2) ops so it is recurrent relation itself