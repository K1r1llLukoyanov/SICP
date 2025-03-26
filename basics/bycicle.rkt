#! /usr/bin/env racket
#lang racket

(define (solve bycicles)
    (if 
        (= bycicles 1) 100
        (+ (/ 100 bycicles) (solve (- bycicles 1)))
    ))

(round (solve 50))