#lang sicp

; Exercise: define or-gate functional element

(define (logical-or a b)
    (cond
        ((= a 0)
            (cond
                ((= b 0) 0)
                ((= b 1) 1)
                (else (error "Wrong type of signal -- LOGICAL-OR" b)))
        ((= a 1)
            (cond
                ((= b 0) 1)
                ((= b 1) 1)
                (else (error "Wrong type of signal -- LOGICAL-OR" b)))
        (else (error "Wrong type of signal -- LOGICAL-OR" a))))))


(define (or-gate in1 in2 out)
    (define (add-action-procedure)
        (let ((new-value (logical-or (get-signal in1) (get-signal in2))))
            (after-delay or-gate-delay
                (lambda () (set-signal! out new-value)))))
    (add-action! in1 add-action-procedure)
    (add-action! in2 add-action-procedure)
    `ok)