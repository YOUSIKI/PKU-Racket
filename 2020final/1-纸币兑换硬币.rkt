#lang racket

(define (sum lst)
  (if (null? lst) 0 (+ (car lst) (sum (cdr lst)))))

(define (solve x y z)
  (define (check i) ; use i y
    (if (= i 0) 0
        (let ((iy (* i y)))
          (let ((remain (- x iy)))
            (if (= 0 remain) 0
                (if (= 0 (remainder remain z)) 1 0))))))
  (begin
    (set! x (* 10 x))
    (sum (map check (range (+ 1 (/ (- x (remainder x y)) y)))))))

(define (main-loop)
  (let ((x (read))
        (y (read))
        (z (read)))
    (if (eq? x eof)
        (void)
        (begin
          (displayln (solve x y z))
          (main-loop)))))

(main-loop)