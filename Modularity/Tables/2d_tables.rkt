#lang sicp

; Implementation of 2 dimensional tables

(define (lookup key-1 key-2 table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (cdr record)
                    false)))))


(define (assoc key records)
    (cond 
        ((null? records) false)
        ((eq? (caar records) key) (car records))
        (else (assoc key (cdr records)))))


(define (insert! key-1 key-2 value table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (set-cdr! record value)
                    (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! table 
                (cons (list key-1 (cons key-2 value))
                      (cdr table))))))

(define (make-table)
    (list `*table*))

(define table (make-table))
(insert! `math `+ 12 table)
(insert! `math `* 13 table)
(lookup `math `+ table) ; prints 12
(lookup `math `* table) ; prints 13
(insert! `math `+ 14 table)
(lookup `math `+ table) ; prints 14

