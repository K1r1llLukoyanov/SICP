#! /usr/bin/env racket
#lang racket

(define (add-element new-element prev-list)
  (cons new-element prev-list))


(define list-end 0)
(define new-list (add-element 10 list-end))
(print-list new-list)

