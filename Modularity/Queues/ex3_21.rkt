#lang sicp

; Exercise: write procedure for queue printing

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))


(define (make-queue) (cons `() `()))

(define (empty-queue? queue)
    (null? (front-ptr queue)))

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT was called for empty queue!" queue)
        (car (front-ptr queue))))

(define (insert-queue! queue item)
    (let ((new-pair (cons item `())))
        (cond 
            ((empty-queue? queue)
             (set-front-ptr! queue new-pair)
             (set-rear-ptr!  queue new-pair)
             queue)
            (else 
             (set-cdr! (rear-ptr queue) new-pair)
             (set-rear-ptr! queue new-pair)
             queue))))

(define (delete-queue! queue)
    (cond 
        ((empty-queue? queue)
           (error "DELETE was called with empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (print-queue queue)
    (front-ptr queue))

(define queue (make-queue))
(insert-queue! queue `a)
(print-queue queue) ; prints (a)
(insert-queue! queue `b)
(print-queue queue) ; prints (a b)
(delete-queue! queue)
(print-queue queue) ; prints (b)
(delete-queue! queue)
(print-queue queue) ; prints ()