#! /usr/bin/env racket
#lang racket


; index operation to get n'th element from beginning of list
(define (list-ref items n)
    (if (= n 0) (car items) (list-ref (cdr items) (- n 1))))

; procedure to get length of list
(define (length items)
    (if (null? items ) 0 (+ 1 (length (cdr items)))))


; map procedure
; takes list and function as arguments and applies this function
; to all elements of list
; returns updated list
(define (map proc lst)
    (define (map-helper old-lst proc new-lst)
        (if (null? old-lst) 
            new-lst
            (map-helper (cdr old-lst) proc (append new-lst (list (proc (car old-lst)))))
        ))
    (map-helper lst proc null))


; filter procedure
; returns list of elements that satisfy given condition
(define (filder proc lst)
    (define (filter-helper old-lst proc new-lst)
        (if (null? old-lst) 
            new-lst
            (if (car old-lst) 
                (filter-helper (cdr old-lst) proc (append new-lst (list (car old-lst))))
                (filter-helper (cdr old-lst) proc new-lst)
            )
        ))
    (filter-helper lst proc null))


; reduce function
; return value that was calculated by applying 
; some function to previously calculated value and current list value
; for first element previous value is initial value
(define (reduce proc lst init-value)
    (if (null? lst) init-value (reduce proc (cdr lst) (proc init-value (car lst)))))


(define (enumerate-interval low high)
    (if (> low high) null (cons low (enumerate-interval (+ low 1) high))))


; reversing list
(define (reverse lst)
    (define (reverse-helper old-lst new-lst)
        (if (null? old-lst)
            new-lst
            (reverse-helper (cdr old-lst) (append (list (car old-lst)) new-lst))
        ))
    (reverse-helper lst null))

; checking is it is last element
(define (next-null? lst)
    (if (null? lst) #f (null? (cdr lst))))


; getting last element of list
(define (last-pair lst)
    (if (null? lst) lst (if (next-null? lst) (car lst) (last-pair (cdr lst)))))


; appending two lists
(define (append list1 list2)
    (if (null? list1) list2 (cons (car list1) (append (cdr list1) list2))))


(define (square-list items)
    (map (lambda (x) (* x x)) items))


(define (scale-list items factor)
    (map (lambda (x) (* x factor)) items))




(define lst (list 1 2 3 4 5))
(define (square x) (* x x))

(define squares (square-list lst))   ; 1 4 9 16 25
(define (add x y) (+ x y))

(reverse squares)                   ; 25 16 9 4 1

(define (abs x) ((if (< x 0) - +) x))

(define olst (list -10 2.5 -11.6 17))

(map abs olst)

