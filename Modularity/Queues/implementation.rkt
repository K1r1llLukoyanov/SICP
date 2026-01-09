#lang sicp

; Implementation of queue (FIFO structure)

; constructor: make-queue returns empty queue
; selectors:
;   empty-queue?    return true if queue is empty
;   front-queue     return front element of queue
;   insert-queue!   pushes element to the back of queue
;   delete-queue!   pops element from head of queue and return new queue

; queue could be implemented using cons, car, cdr
; but with this approach access to the last element of
; queue will be ineffective

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
             (set-set-rear-ptr! queue new-pair)
             queue))))

(define (delete-queue! queue)
    (cond 
        ((empty-queue? queue)
           (error "DELETE was called with empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue))
         queue))))