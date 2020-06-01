#lang racket

(define (main-loop)
  (let ((input (read)))
    (if (eq? eof input)
        (void)
        (begin (displayln (flatten input))
               (main-loop)))))

(main-loop)