#lang racket

(define (super-fibonacci n)
  (define (super-fibonacci-iter rest Fn-1 Fn-2 Fn-3 Fn-4 Fn-5)
    (let ((Fn (+ Fn-1 (* 4 Fn-2) (* 5 Fn-3) (* -2 Fn-4 Fn-4) (* Fn-5 Fn-5 Fn-5))))
      (if (= rest 1) Fn (super-fibonacci-iter (- rest 1) Fn Fn-1 Fn-2 Fn-3 Fn-4))))
  (if (< n 5) 1 (super-fibonacci-iter (- n 4) 1 1 1 1 1)))

(define (main-loop)
  (let ((input (read)))
    (if (eq? eof input)
        (void)
        (begin (displayln (super-fibonacci input))
               (main-loop)))))

(main-loop)