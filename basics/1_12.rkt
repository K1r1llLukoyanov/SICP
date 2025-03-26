#! /usr/bin/env racket
#lang racket

(define (pascal-recursive d l)
    (cond
        ((or (= l 0) (= d l)) 1)
        (else
            (+ (pascal-recursive (- d 1) l) (pascal-recursive (- d 1) (- l 1)))
        )
    )
)


