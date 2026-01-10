#lang sicp

; Queue

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


; Digital schemes simulation

(define (call-each procedures)
    (if (null? procedures)
        `done
        (begin ((car procedures)) (call-each (cdr procedures)))))

(define (make-wire)
    (let ((signal-value 0) (action-procedure `()))
        (define (set-my-signal! new-value)
            (if (not (= signal-value new-value))
                (begin (set! signal-value new-value)
                       (call-each action-procedure))
                `done))
        (define (accept-action-procedure! proc)
            (set! action-procedure (cons proc action-procedure))
            (proc))
        (define (dispatch m)
            (cond
                ((eq? m `get-signal) signal-value)
                ((eq? m `set-signal!) set-my-signal!)
                ((eq? m `add-action!) accept-action-procedure!)
                (else (error "Undefined behavior of wire object!" m))))
    dispatch))

(define (get-signal wire)
    (wire `get-signal))

(define (set-signal! wire new-value)
    ((wire `set-signal!) new-value))

(define (add-action! wire proc)
    ((wire `add-action!) proc))

(define (logical-not value)
    (cond 
        ((= value 0) 1)
        ((= value 1) 0)
        (else (error "Wrong type of signal -- LOGICAL-NOT!" value))))

(define (logical-and a b)
    (cond
        ((= a 0)
            (cond
                ((= b 0) 0)
                ((= b 1) 0)
                (else (error "Wrong type of signal -- LOGICAL-AND" b))))
        ((= a 1)
            (cond
                ((= b 0) 0)
                ((= b 1) 1)
                (else (error "Wrong type of signal -- LOGICAL-AND" b))))
        (else (error "Wrong type of signal -- LOGICAL-AND" a))))

(define (logical-or a b)
    (cond
        ((= a 0)
            (cond
                ((= b 0) 0)
                ((= b 1) 1)
                (else (error "Wrong type of signal -- LOGICAL-OR" b))))
        ((= a 1)
            (cond
                ((= b 0) 1)
                ((= b 1) 1)
                (else (error "Wrong type of signal -- LOGICAL-OR" b))))
        (else (error "Wrong type of signal -- LOGICAL-OR" a))))

(define (inverter in out)
    (define (invert-input)
        (let ((new-value (logical-not (get-signal in))))
            (after-delay inverter-delay
                (lambda () (set-signal! out new-value)))))
    (add-action! in invert-input)
    `ok)

(define (and-gate in1 in2 out)
    (define (add-action-procedure)
        (let ((new-value (logical-and (get-signal in1) (get-signal in2))))
            (after-delay and-gate-delay
                (lambda () (set-signal! out new-value)))))
    (add-action! in1 add-action-procedure)
    (add-action! in2 add-action-procedure)
    `ok)

(define (or-gate in1 in2 out)
    (define (add-action-procedure)
        (let ((new-value (logical-or (get-signal in1) (get-signal in2))))
            (after-delay or-gate-delay
                (lambda () (set-signal! out new-value)))))
    (add-action! in1 add-action-procedure)
    (add-action! in2 add-action-procedure)
    `ok)


(define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)
        `ok))

(define (full-adder a b c-in sum c-out)
    (let 
        ((s (make-wire))
         (c1 (make-wire))
         (c2 (make-wire)))
        (half-adder b c-in s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 c-out)
        `ok))


; Implementation of actions schedule (agenda)
; agenda object is consist of such constructors 
; and selectors:
; (make-agenda) returns empty agenda
; (empty-agenda? <the-agenda>) returns true if the-agenda is empty
; (first-agenda-item <the-agenda>) return first item of the-agenda
; (remove-first-agenda-item! <the-agenda>) removes first item of the-agenda
; (add-to-agenda! <time> <action> <the-agenda>) add action <action>
; with <time> delay into <the-agenda> object
; (current-time <the-agenda>) returns current time of <the-agenda> object

(define (after-delay delay action)
    (add-to-agenda! (+ delay (current-time the-agenda))
                    action
                    the-agenda))

(define (propagate)
    (if (empty-agenda? the-agenda)
        `done
        (let ((first-item (first-agenda-item the-agenda)))
            (first-item)
            (remove-first-agenda-item! the-agenda)
            (propagate))))

(define (make-time-segment time queue)
    (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time) (set-car! agenda time))

(define (segments agenda)
    (cdr agenda))

(define (set-segments! agenda segs)
    (set-cdr! agenda segs))

(define (first-segment agenda)
    (car (segments agenda)))

(define (rest-segments agenda)
    (cdr (segments agenda)))

(define (empty-agenda? agenda)
    (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
    (define (belongs-before? segments)
        (or (null? segments)
            (< time (segment-time (car segments)))))
    (define (make-new-time-segment time action)
        (let ((q (make-queue)))
            (insert-queue! q action)
            (make-time-segment time q)))
    (define (add-to-segments! segments)
        (if (= (segment-time (car segments)) time)
            (insert-queue! (segment-queue (car segments)) action)
            (let ((rest (cdr segments)))
                (if (belongs-before? rest)
                    (set-cdr! segments 
                            (cons 
                                (make-new-time-segment time action)
                                rest))
                    (add-to-segments! rest)))))
    (let ((segments (segments agenda)))
        (if (belongs-before? segments)
            (set-segments! agenda (cons (make-new-time-segment time action) segments))
            (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
    (let ((q (segment-queue (first-segment agenda))))
        (delete-queue! q)
        (if (empty-queue? q)
            (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
    (if (empty-agenda? agenda)
        (error "first-agenda-item was called with empty agenda" agenda)
        (let ((first-seg (first-segment agenda)))
            (set-current-time! agenda (segment-time first-seg))
            (front-queue (segment-queue first-seg)))))


(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (probe name wire)
    (add-action! wire 
                (lambda ()
                    (display name)
                    (display " ")
                    (display (current-time the-agenda))
                    (display " New-value = ")
                    (display (get-signal wire))
                    (newline))))

(define input1 (make-wire))
(define input2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe `input1 input1)
(probe `input2 input2)
(probe `sum sum)
(probe `carry carry)

(half-adder input1 input2 sum carry)
(set-signal! input1 1)
(set-signal! input2 1)
(propagate) ; prints sum = 0, carry = 1
