#lang sicp

; Implementation of infinite lazy streams

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
  (newline)
  (display x))

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


(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1)))) ; infinite stream

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
    (stream-filter (lambda (value) (not (divisible? value 7))) (integers-starting-from 6)))

(define (fibgen a b)
    (cons-stream a (fibgen b (+ a b))))

(stream-car no-sevens)
(stream-car (stream-cdr no-sevens))

(define (sieve stream)
    (cons-stream
        (stream-car stream)
        (sieve (stream-filter (lambda (value) 
            (not (divisible? value (stream-car stream))))
            (stream-cdr stream)))))

(define erato (sieve (integers-starting-from 2)))
(stream-ref erato 50) ; 233

; implicit definition of stream

(define ones (cons-stream 1 ones)) ; infinite stream of ones
; it has 1 as car
; and promise to return ones as cdr

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define ints (cons-stream 1 (add-streams ones ints)))

(stream-ref ints 10)

(define fibs (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(stream-ref fibs 11); 89

(define (scaled-stream stream factor)
    (cons-stream (* (stream-car stream) factor) (scaled-stream (stream-cdr stream) factor)))

(define power2 (cons-stream 1(scaled-stream power2 2)))

(stream-ref power2 0); 1
(stream-ref power2 10); 1024

(define (square x) (* x x))

(define (prime? n)
    (define (iter ps)
        (cond ((> (square (stream-car ps)) n) true)
            ((divisible? n (stream-car ps)) false)
            (else (iter (stream-cdr ps)))))
    (iter primes))

(define primes (cons-stream 2 (stream-filter prime? (integers-starting-from 3))))
(stream-ref primes 10)

(define (mul-streams stream1 stream2)
    (stream-map * stream1 stream2))

(define factorials (cons-stream 1 (mul-streams (integers-starting-from 1) factorials)))

(stream-ref factorials 6)

(define (partial-sums stream)
    (define new-stream
        (cons-stream 
            (stream-car stream) 
            (stream-map + (stream-cdr stream) new-stream)))
    new-stream)

(define part (partial-sums integers))
(stream-ref part 4)

(define (generate-next stream)
    (define (iter counter)
        (if (= counter 0)
            (generate-next (stream-cdr stream))
            (cond 
                ((= counter 3) 
                    (cons-stream 
                        (* (stream-car stream) 2)
                        (iter (- counter 1))))
                ((= counter 2)
                    (cons-stream
                        (* (stream-car stream) 3)
                        (iter (- counter 1))))
                ((= counter 1)
                    (cons-stream
                        (* (stream-car stream) 5)
                        (iter (- counter 1)))))
        ))
    (iter 3))


(define (merge-streams stream1 stream2)
    (cond 
        ((stream-null? stream1) stream2)
        ((stream-null? stream2) stream1)
        (else
            (cond ((eq? (stream-car stream1) (stream-car stream2))
                    (cons-stream 
                        (stream-car stream1)
                        (merge-streams (stream-cdr stream1) (stream-cdr stream2))))
                ((< (stream-car stream1) (stream-car stream2))
                    (cons-stream
                        (stream-car stream1)
                        (merge-streams (stream-cdr stream1) stream2)))
                (else
                    (cons-stream
                        (stream-car stream2)
                        (merge-streams stream1 (stream-cdr stream2))))
            ))))

(define hamming-stream
    (cons-stream 1
        (merge-streams
            (merge-streams  
                (scaled-stream hamming-stream 2) 
                (scaled-stream hamming-stream 3)) 
            (scaled-stream hamming-stream 5))))

(stream-ref hamming-stream 123)

(define (expand num den radix)
    (cons-stream
        (quotient (* num radix) den)
        (expand (remainder (* num radix) den) den radix)))

(define expanded (expand 5 100 10))
(stream-ref expanded 0)
(stream-ref expanded 1)

(define (integrate-series coeffs)
    (define (iter stream series-num)
        (cons-stream 
            (/ (stream-car stream) series-num)
            (iter (stream-cdr stream) (+ series-num 1))))
    (iter coeffs 1))

(define integrated (integrate-series integers))
(stream-ref integrated 2)


(define exp-series
    (cons-stream 1 (integrate-series exp-series)))

(define cosine-stream
    (cons-stream 1 (integrate-series (stream-map - sine-stream))))

(define sine-stream
    (cons-stream 0 (integrate-series cosine-stream)))

(stream-ref cosine-stream 4)