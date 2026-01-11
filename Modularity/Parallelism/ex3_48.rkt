#lang sicp

; Exercise: implement serialized-exchange and get rid of deadlocks

(define (make-balance-and-serializer balance id)
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
                ((eq? m `get-id) id)
                (else (error "Undefined behavior -- MAKE-ACOUNT" m))))
        dispatch))

(define (exchange account1 account2)
    (let ((difference (- (account1 `balance) (account2 `balance))))
        ((account1 `withdraw) difference)
        ((account2 `deposit) difference)))

(define (serialized-exchange account1 account2)
    (let ((serializer1 (account1 `serializer))
        (serializer2 (account2 `serializer))
        (id1 (account1 `get-id))
        (id2 (account2 `get-id)))
        (if (< id1 id2)
            ((serializer1 (serializer2 exchange)) account1 account2)
            ((serializer2 (serializer1 exchange)) account1 account2))
        ))

