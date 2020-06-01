#lang racket

(define (fast-power a n)
  (define (fast-power-iter a n product)
    (cond ((= 0 n) product)
          ((odd? n) (fast-power-iter a (- n 1) (* a product)))
          ((even? n) (fast-power-iter (* a a) (/ n 2) product))))
  (fast-power-iter a n 1))

(define (main-loop)
  (let ((a (read))
        (n (read)))
    (if (eq? eof a)
        (void)
        (begin (displayln (fast-power a n))
               (main-loop)))))

(main-loop)