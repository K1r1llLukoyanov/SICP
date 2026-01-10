#lang sicp

; Implementation of table objects

(define (make-table)
    (let ((local-table (list `*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (cdr record)
                            false))
                    false)))
        (define (assoc key table)
            (cond 
                ((null? table) false)
                ((eq? (caar table) key) (car table))
                (else (assoc key (cdr table)))))
        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
                    (set-cdr! local-table (cons (list key-1 (cons key-2 value)) (cdr local-table))))))
        (define (dispatch m)
            (cond 
                ((eq? m `insert!) insert!)
                ((eq? m `lookup) lookup)
                ((eq? m `assoc) assoc)
                (else (error "Undefined behavior of table object!"))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table `lookup))
(define put (operation-table `insert!))

(put `math `+ 12)
(get `math `+)
