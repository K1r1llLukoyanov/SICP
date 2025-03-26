#! /usr/bin/env racket
#lang racket

(define (eval left op right)
    (op left right))


(eval (eval 4 + 4) * (eval 2 + 2))