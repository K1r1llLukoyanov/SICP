#! /usr/bin/env racket
#lang racket


; representing of segment on a line
; (start-segment, end-segment)
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)


; representing a point on a plane
; (x, y)

(define make-point cons)
(define x-point car)
(define y-point cdr)


(define (mid-point-segment segment)
  (/ (+ (start-segment segment) (end-segment segment)) 2))

(define (print-point p)
  (writeln)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (writeln ")"))

(define segment (make-segment 2 6))
(mid-point-segment segment)
