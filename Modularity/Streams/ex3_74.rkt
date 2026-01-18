#lang sicp

; Exercise: Make function which detects sign chaging

(define zero-crossing
    (stream-map sign-change-detector sense-data (stream-cdr sense-data)))

