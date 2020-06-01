#lang racket

(define (main-loop)
  (let ((input (read)))
    (if (eq? eof input)
        (void)
        (begin (displayln (reverse input))
               (main-loop)))))

(main-loop)