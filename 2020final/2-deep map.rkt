#lang racket

(define (sqr lst)
  (if (list? lst)
      (map sqr lst)
      (* lst lst)))

(define (main-loop)
  (let ((input (read)))
    (if (eq? input eof)
        (void)
        (begin
          (displayln (sqr input))
          (main-loop)))))

(main-loop)