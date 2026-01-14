#lang sicp

; Implementation of lazy streams

(define the-empty-stream `())
(define (stream-null? stream) (null? stream))

(define (stream-car stream) (car stream))
(define (force proc) (proc))

(define (stream-cdr stream) (force (cdr stream)))

(define (memo-proc proc)
    (let ((already-run? false) (result false))
        (lambda ()
            (if (not already-run?)
                (begin (set! result (proc))
                (set! already-run? true)
                result)
                result))))

(define (any proc lst)
    (if (null? lst)
        false
        (if (proc (car lst))
            true
            (any proc (cdr lst)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (memo-proc (lambda () b))))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (stream-map proc argstreams)
    (if (stream-null? argstreams)
        the-empty-stream
        (cons-stream 
            (proc (stream-car argstreams))
            (stream-map proc (stream-cdr argstreams)))))

(define (show x)
    (display-line x)
    x)

(define (display-line x)
    (display x)
    (newline))

(define x (stream-map show (stream-enumerate-interval 0 10))) ; 0
(newline)
(stream-ref x 5)    ; 1
                    ; 2
                    ; 3
                    ; 4
                    ; 5
                    ; 5 <=
(newline)
(stream-ref x 7)    ; 6
                    ; 7
                    ; 7 <=