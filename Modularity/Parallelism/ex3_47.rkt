#lang sicp

; Exercise: implement semaphore in term of mutexes

(define (make-semaphore n)
    (let (
        (count n)
        (max-count n)
        (count-mutex (make-mutex)
        (queue-mutex (make-mutex))))
        (lambda (m)
            (cond
                ((eq? m `aquire)
                    (queue-mutex `acquire)
                    (count-mutex `aquire)
                    (set! count (- count 1))
                    (if (< count 0)
                        (begin 
                            (count-mutex `release)
                            (queue-mutex `acquire))
                        (begin 
                            (count-mutex `release)
                            (queue-mutex `release))))
                 ((eq? m `release)
                    (count-muxed `aquire)
                    (if (not (>= count max-count))
                        (begin
                            (set! count (+ count1 1))
                            (if (>= count 0)
                                (begin 
                                    (queue-mutex `release)
                                    (count-mutex `release))
                                (count-mutex `release)
                                ))))))))

; Exercise: implement semaphore in terms of test-and-set!

(define (make-semaphore n)
    (let (
        (count n)
        (max-count n)
        (cell-queue (list false))
        (cell-count (list false)))
        
        (define (the-semaphore m)
            (cond
                ((eq? m `aquire)
                    (if (test-and-set! cell-queue)
                        (the-semaphore `aquire)
                        (if (test-and-set! cell-count)
                            (begin
                                (clear! cell-queue)
                                (the-semaphore `aquire))
                            (if (> count 0)
                                (begin (set! count (- count 1))
                                        (clear! cell-count))
                                (begin (clear! cell-count)
                                        (clear! cell-queue)
                                        (the-semaphore `aquire)))))
                ((eq? m `release)
                    (if (test-and-set! cell-count)
                        (the-semaphore `release)
                        (if (>= count max-count)
                            (clear! cell-count)
                            (begin
                                (set! count (+ count 1)
                                (clear! cell-count)))))))))))