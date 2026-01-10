#lang sicp

; Exercise: create table that organizes its data in tree using
; some comparison function for quick search

(define (create-node key value left right)
    (cons (cons key value) (cons left right)))

(define (get-node-key node)
    (if (null? node)
        `()
        (caar node)))

(define (get-node-value node)
    (if (null? node)
        `()
        (cadr node)))

(define (get-node-left node)
    (if (null? node)
        `()
        (cdar node)))

(define (get-node-right node)
    (if (null? node)
        `()
        (cddr node)))

(define (set-node-value! node item)
    (set-cdr! (car node) item))

(define (set-node-left! node item)
    (set-car! (cdr node) item))

(define (set-node-right! node item)
    (set-cdr! (cdr node) item))

(define (make-table comp)
    (let ((local-table (list `*table*)))
        (define (lookup key)
            (let ((record (assoc key (cdr local-table))))
                (if (null? record)
                    false
                    (get-node-value record))))
        (define (assoc key node)
            (cond 
                ((null? node) false)
                (let ((comp-value (comp (get-node-key node) key)))
                    (cond 
                        ((= comp-value 0) node)
                        ((< comp-value 0) (assoc key (get-node-left node)))
                        (else (assoc key (get-node-right node)))))))
        (define (get-leaf-node key current)
            (let ((comp-value (comp (get-node-key current) key)))
                (cond 
                    ((= comp-value 0) (cons `replace current))
                    ((< comp-value 0) 
                        (if (null? (get-node-left current))
                            (cons `left current)
                            (get-leaf-node key (get-node-left current)))
                    (else
                        (if (null? (get-node-right current))
                            (cons `right current)
                            (get-leaf-node key (get-node-right current))))))))
        (define (insert key value)
            (let ((new-record (create-node key value `() `())))
                (cond 
                    ((null? (cdr local-table))
                     (set-cdr! local-table new-record))
                    (else
                        (let ((leaf-node (get-leaf-node key (cdr table))))
                            (cond
                                ((eq? (car leaf-node) `replace)
                                 (set-node-value! (cdr leaf-node) new-record))
                                ((eq? (car leaf-node) `left)
                                 (set-node-left! (cdr leaf-node) new-record))
                                ((eq? (car leaf-node) `right)
                                 (set-node-right! (cdr leaf-node) new-record))))))))
        (define (dispatch m)
            (cond
                ((eq? m `lookup) lookup)
                ((eq? m `insert!) insert)
                (else (error "Undefined behavior of table object!" m))))
        dispatch))

