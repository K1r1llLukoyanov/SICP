#lang sicp


(define (square x) (* x x))

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

(define (display-line x)
  (display x)
  (newline))

(define (display-first s n)
    (if (= n 0)
        `done
        (begin 
            (display-line (stream-car s))
            (display-first (stream-cdr s) (- n 1)))))

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


(define (stream-map proc . argstreams)
  (if (any stream-null? argstreams)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (scaled-stream stream factor)
    (cons-stream (* (stream-car stream) factor) (scaled-stream (stream-cdr stream) factor)))

(define (sum-first-terms stream term-num)
    (define (iter result pos counter)
        (if (= counter term-num)
            result
            (iter (+ result (stream-car pos)) (stream-cdr pos) (+ counter 1))))
    (iter 0 stream 0))


(define (add-streams s1 s2)
    (stream-map + s1 s2))


(define (integrate integrand initial-value dt)
    (define int
        (cons-stream initial-value 
            (add-streams int (scaled-stream integrand dt))))
    int)

(define (integral delayed-integrand initial-value dt)
    (define int (cons-stream initial-value
        (let ((integrand (force delayed-integrand)))
            (add-streams int (scaled-stream integrand dt)))))
    int)

(define (solve f y0 dt)
    (define y (integral (delay dy) y0 dt))
    (define dy (stream-map f y))
    y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
