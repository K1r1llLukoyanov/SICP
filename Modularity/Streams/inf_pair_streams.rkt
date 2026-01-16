#lang sicp

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

(define (merge-pairs-stream p1 p2)
    (cond
        ((stream-null? p1) p2)
        ((stream-null? p2) p1)
        ((and (stream-null? p1) (stream-null? p2)) the-empty-stream)
        (else
            (let ((sum1 (+ (car (stream-car p1)) (cadr (stream-car p1))))
                  (sum2 (+ (car (stream-car p2)) (cadr (stream-car p2)))))
                (cond 
                    ((= sum1 sum2)
                        (if (< (car (stream-car p1)) (car (stream-car p2)))
                            (begin 
                                (cons-stream (stream-car p1)
                                    (merge-pairs-stream (stream-cdr p1) p2)))
                            (begin
                                (cons-stream (stream-car p2)
                                    (merge-pairs-stream p1 (stream-car p2))))))
                    ((< sum1 sum2)
                        (cons-stream (stream-car p1)
                            (merge-pairs-stream (stream-cdr p1) p2)))
                    (else
                        (cons-stream (stream-car p2)
                            (merge-pairs-stream p1 (stream-cdr p2)))))))))

(define (pairs s t)
    (cons-stream 
        (list (stream-car s) (stream-car t))
        (merge-pairs-stream
            (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
            (pairs (stream-cdr s) (stream-cdr t)))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 0 (stream-map + ones integers)))

(define diff-pairs (pairs integers integers))

(display-first diff-pairs 100)
