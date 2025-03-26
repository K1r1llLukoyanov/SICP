#! /usr/bin/env racket
#lang racket

(define (make-mobile left right)
    (list left right))


(define (make-branch length structure)
    (list length structure))


(define (left-branch bin-mobile)
    (car bin-mobile))


(define (right-branch bin-mobile)
    (cdr bin-mobile))


(define (branch-length branch)
    (car branch))


(define (branch-structure branch)
    (cdr branch))


(define (total-weight mobile)
    (cond 
        ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else 
            (+  (* (branch-length (left-branch mobile)) 
                    (total-weight (branch-structure (left-branch mobile))))
                (* (branch-length (right-branch mobile)) 
                    (total-weight (branch-structure (right-branch mobile))))))
    ))


(define (is-balanced? mobile)
    (cond 
        ((null? mobile) #t)
        ((pair? mobile) #t)
        (else 
            (=  (*  (branch-length (left-branch mobile)) 
                    (total-weight (branch-structure (left-branch mobile)))) 
                (*  (branch-length (right-branch mobile))
                    (total-weight (branch-structure (right-branch mobile))))))
    ))


