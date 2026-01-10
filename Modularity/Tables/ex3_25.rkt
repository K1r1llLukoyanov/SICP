#lang sicp

; Exercise: implement table with variable number of keys

(define (lookup keys table)
    (let ((subtable (assoc (car keys) (cdr table))))
        (if subtable
            (if (null? (cdr keys))
                (cdr subtable)
                (lookup (cdr keys) subtable)))))


(define (assoc key records)
    (cond 
        ((null? records) false)
        ((eq? (caar records) key) (car records))
        (else (assoc key (cdr records)))))

(define (create-sub-table keys value)
    (if (null? (cdr keys))
        (cons (car keys) value)
        (list (car keys) (create-sub-table (cdr keys) value))))

(define (insert! keys value table)
    (let ((subtable (assoc (car keys) (cdr table))))
        (if subtable
            (if (null? (cdr keys))
                (set-cdr! subtable value)
                (insert! (cdr keys) value subtable))
            (set-cdr! table (cons (create-sub-table keys value) (cdr table))))))

(define (make-table)
    (list `*table*))

(define table (make-table))
(insert! (list `a `b `c) 10 table)
(lookup (list `a `b `c) table) ; prints 10