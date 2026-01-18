#lang sicp

; Exercise: write merge function which takes function weight
; which return weight of pair
; merge function should sort pairs based on weight of pair

(define (sum p)
    (+ (car p) (cadr p)))

(define (merge-weight weight p1 p2)
    (cond
        ((stream-null? p1) p2)
        ((stream-null? p2) p1)
        ((and (stream-null? p1) (stream-null? p2)) the-empty-stream)
        (else
            (let ((weight1 (weight (stream-car p1)))
                  (weight2 (weight (stream-car p2))))
                (cond 
                    ((= weight1 weight2)
                        (if (< (car (stream-car p1)) (car (stream-car p2)))
                            (begin 
                                (cons-stream (stream-car p1)
                                    (merge-weight weight (stream-cdr p1) p2)))
                            (begin
                                (cons-stream (stream-car p2)
                                    (merge-weight weight p1 (stream-car p2))))))
                    ((< weight1 weight2)
                        (cons-stream (stream-car p1)
                            (merge-weight weight (stream-cdr p1) p2)))
                    (else
                        (cons-stream (stream-car p2)
                            (merge-weight weight p1 (stream-cdr p2)))))))))

(define (weighted-pairs weight s t)
    (cons-stream 
        (list (stream-car s) (stream-car t))
        (merge-pairs-stream weight
            (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
            (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 0 (stream-map + ones integers)))

; a) weighted by sum i + j
(define sum-weighed (weighted-pairs sum integers integers))

(define (check-num num)
    (not (or
            (= (remainder num 2) 0)
            (= (remainder num 3) 0)
            (= (remainder num 5) 0))))

(define (filter-func pair)
    (and (check-num (car pair) (check-num (cadr pair))))

(define (weight p)
    (+ (* 2 (car p)) (* 3 (cadr p)) (* 5 (car p) (cadr p))))


; b) weighed by 2*i + 3*j + 5*i*j
(define weighed (filter-stream
    filter-func (weighted-pairs (pairs weight integers integers))))