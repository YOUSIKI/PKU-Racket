#lang racket

(define exit #f)

(define cont-list '())
(define len (read))

(define (set-cont-list n)
  (set! cont-list
        (map (lambda (i) (creater (+ i 1)) continuation) (range n))))

(define continuation #f)

(define (creater n)
  (if (call/cc (lambda (c) (set! continuation c) #t))
      (void)
      (printer n)))

(define (printer n)
  (if (= n 0) (void)
      (begin (displayln n)
             (printer (- n 1)))))
  
(define (show n)
  (define (show-helper l n)
    (if (= n 0)
        (if (continuation? (car l))
            ((car l) #f)
            (displayln "error"))
        (show-helper (cdr l) (- n 1))))
  (show-helper cont-list (- n 1)))

(define (main)
  (set-cont-list len)
  (define k (read))
  (if (eq? k eof)
      (void)
      (show k)))

(main)