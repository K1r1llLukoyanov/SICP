#lang sicp

; Exercice: generate infinite stream of triples (S_i, T_j, U_k) from 
; streams S, T, U where i <= j <= k. Then from this stream generate
; infinite stream of Pythagoras triplets

(define (pairs s t)
    (cons-stream 
        (list (stream-car s) (stream-car t))
        (merge-pairs-stream
            (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
            (pairs (stream-cdr s) (stream-cdr t)))))


(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
            (interleave s2 (stream-cdr s1)))))

(define (triplets s t u)
    (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
        (interleave 
            (interleave
                (stream-map (lambda (x) (list (stream-car s) (stream-car t) x)) (stream-cdr u))
                (stream-map (lambda (x) (list (stream-car s) (car x) (cadr x))) (pairs (stream-cdr t) (stream-cdr u))))
            (triplets (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (pythagoras-stream triplets)
    (stream-filter 
        (lambda (x) (= (square (caddr x)) (+ (square (car x)) (square (cadr x))))) 
        triplets))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 0 (stream-map + ones integers)))

(define diff-triplets (triplets integers integers integers))
(define pythagoras (pythagoras-stream diff-triplets))

