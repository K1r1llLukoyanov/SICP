#lang sicp

; Exercise: implement deque (double-side queue)

(define (make-double-ptr data prev next)
    (cons data (cons prev next)))

(define (get-next dptr)
    (if (null? dptr)
        `()
        (cdr (cdr dptr))))

(define (get-prev dptr)
    (if (null? dptr)
        `()
        (car (cdr dptr))))

(define (get-value dptr)
    (if (null? dptr)
        `()
        (car dptr)))

(define (set-value! dptr item)
    (set-car! dptr item))

(define (set-prev! dptr item)
    (set-car! (cdr dptr) item))

(define (set-next! dptr item)
    (set-cdr! (cdr dptr) item))

(define (front-ptr deque) (car deque))

(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))

(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (make-deque) (cons `() `()))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
    (cond
        ((empty-deque? deque)
         (error "front-deque was called for empty deque!"))
        (else (get-value (front-ptr deque)))))

(define (rear-deque deque)
    (cond
        ((empty-deque? deque)
         (error "rear-deque was called for empty deque!"))
        (else (get-value (rear-ptr deque)))))

(define (front-insert-deque! deque item)
    (let ((new-pair (make-double-ptr item `() (front-ptr deque))))
        (cond
            ((empty-deque? deque)
             (set-front-ptr! deque new-pair)
             (set-rear-ptr! deque new-pair)
             deque)
            (else
             (set-prev! (front-ptr deque) new-pair)
             (set-front-ptr! deque new-pair)
             deque))))

(define (rear-insert-deque! deque item)
    (let ((new-pair (make-double-ptr item (rear-ptr deque) `())))
        (cond
            ((empty-deque? deque)
             (set-front-ptr! deque new-pair)
             (set-rear-ptr! deque new-pair)
             (deque))
            (else
             (set-next! (rear-ptr deque) new-pair)
             (set-rear-ptr! deque new-pair)
             deque))))

(define (front-delete-deque! deque)
    (cond 
        ((empty-deque? deque)
         (error "front-delete-deque was called for empty deque!" deque))
        (else
         (set-front-ptr! deque (get-next (front-ptr deque)))
         (set-prev! (front-ptr deque) `())
         deque)))

(define (rear-delete-deque! deque)
    (cond
        ((empty-deque? deque)
         (error "rear-delete-deque was called for empty deque!" deque))
        (else
         (set-rear-ptr! deque (get-prev (rear-ptr deque)))
         (set-next! (rear-ptr deque) `())
         deque)))

(define (print-deque deque)
    (define (create-list-of-items ptr)
        (if (null? ptr)
            `()
            (cons (get-value ptr) (create-list-of-items (get-next ptr)))))
    (create-list-of-items (front-ptr deque)))

(define deque (make-deque))
(front-insert-deque! deque `a)
(front-insert-deque! deque `b)
(front-insert-deque! deque `c)
(rear-insert-deque! deque `d)
(print-deque deque) ; prints (c b a d)
(front-delete-deque! deque)
(print-deque deque) ; prints (b a d)
(rear-delete-deque! deque)
(print-deque deque) ; prints (b a)