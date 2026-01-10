#lang sicp

; Implementation of table (key-value map) structure

(define (assoc key records)
    (cond
        ((null? records) false)
        ((equal? (caar records) key) (car records))
        (else (assoc key (cdr records)))))


(define (lookup key table)
    (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            false)))

(define (insert! key value table)
    (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (set-cdr! table (cons (cons key value) (cdr table))))))

(define (make-table)
    (list `*table*))

(define table (make-table))
(insert! `a 2 table)
(lookup `a table)
