#lang sicp

(define x 10)

; this execution can have 5 different results
(parallel-execute   (lambda () (set! x (* x x)))
                    (lambda () (set! x (+ x 1))))


; operations can be syncronized using serializers
(define s (make-serializer))
; this execution can now have only two resulsts
; x = 101
; x = 121
(parallel-execute   (s (lambda () (set! x (* x x))))
                    (s (lambda () (set! x (+ x 1)))))

; make-balance version with protected balance operations
(define (make-balance-and-serializer balance)
    (define (withdraw amount)
        (if (> amount balance)
            (error "Not enough money on balance!" balance)
            (begin (set! balance (- balance amount)) balance)))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (let ((balance-serializer (make-serializer)))
        (define (dispatch m)
            (cond 
                ((eq? m `withdraw) withdraw)
                ((eq? m `deposit) deposit)
                ((eq? m `balance) balance)
                ((eq? m `serializer) protected)
                (else (error "Undefined behavior -- MAKE-ACOUNT" m))))
        dispatch))

(define (deposit account amount)
    (let ((s (account `serializer))
          (d (account `deposit)))
        ((s d) amount)))

(define (exchange account1 account2)
    (let ((difference (- (account1 `balance) (accoun2 `balance))))
        ((account1 `withdraw) difference)
        ((account2 `deposit) difference)))

(define (serializer-exchange account1 account2)
    (let ((serializer1 (account1 `serializer))
          (serializer2 (account2 `serializer)))
        ((serializer1 (serializer2 exchange)) 
          account1
          account2)))

