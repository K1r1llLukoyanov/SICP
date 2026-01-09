#lang sicp

; Exercise: create make-queue function that
; saves its state in closure environemnt

(define (make-queue)
    (let ((front-ptr `())
          (rear-ptr `()))
        (define (set-front-ptr! item)
            (set! front-ptr item))
        (define (set-rear-ptr! item)
            (set! rear-ptr item))
        (define (empty)
            (null? front-ptr))
        (define (front)
            (if (empty)
                (error "front was called for empty queue!")
                (car front-ptr)))
        (define (insert item)
            (let ((new-pair (cons item `())))
                (cond
                    ((empty)
                     (set-front-ptr! new-pair)
                     (set-rear-ptr! new-pair)
                     (cons front-ptr rear-ptr))
                    (else
                     (set-cdr! rear-ptr new-pair)
                     (set-rear-ptr! new-pair)
                     (cons front-ptr rear-ptr)))))
        (define (delete)
            (cond
                ((empty)
                 (error "delete was called for empty queue!"))
                (else
                 (set-front-ptr! (cdr front-ptr))
                 (cons front-ptr rear-ptr))))
        (define (dispatch m)
            (cond ((eq? m `delete) delete)
                  ((eq? m `insert) insert)
                  ((eq? m `empty) empty)
                  ((eq? m `front) front)))
    dispatch))


(define queue (make-queue))

((queue `insert) `a) ; prints ((a) a)
((queue `insert) `b) ; prints ((a b) b)
((queue `delete))    ; prints ((b) b)