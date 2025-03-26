#! /usr/bin/env racket
#lang racket

; ==================
; creatign binary tree node
; constructor of node
(define (create-node data-value left-subtree right-subtree)
    (cons data-value (cons left-subtree right-subtree)))
; ==================


; ==================
; accessors of node
(define (get-data node)
    (car node))

(define (get-left-subtree node)
    (cond 
        ((null? node) null)
        ((pair? node) (car (cdr node)))
        (else node)
    ))

(define (get-right-subtree node)
    (cond 
        ((null? node) null)
        ((pair? node) (cdr (cdr node)))
        (else node)
    ))
; ==================


; ==================
; tree traversing procedures
(define (print-inorder node)
    (if (null? node) (display "") (print-inorder (get-left-subtree node)))
    (if (null? node) (display "") (writeln (get-data node)))
    (if (null? node) (display "") (print-inorder (get-right-subtree node))))

(define (print-outorder node)
    (if (null? node) (display "") (print-inorder (get-right-subtree node)))
    (if (null? node) (display "") (writeln (get-data node)))
    (if (null? node) (display "") (print-inorder (get-left-subtree node))))

(define (print-medium node)
    (if (null? node) (display "") (writeln (get-data node)))
    (if (null? node) (display "") (print-inorder (get-left-subtree node)))
    (if (null? node) (display "") (print-inorder (get-right-subtree node))))
; ==================


; getting node count
(define (count-nodes root)
    (if (null? root) 
        0 
        (+ 1 (count-nodes (get-left-subtree root)) 
            (count-nodes (get-right-subtree root)))))


; getting max depth
(define (max-depth root)
    (if (null? root)
        0
        (+ 1 (max (max-depth (get-left-subtree root)) (max-depth (get-right-subtree root))))))


(define (map proc tree)
    (if (null? tree)
        null
        (create-node (proc (get-data tree)) (map proc (get-left-subtree tree)) (map proc (get-right-subtree tree)))
    ))


(define tree (create-node 2 (create-node 1 null null) (create-node 3 null null)))

(print-inorder tree)
