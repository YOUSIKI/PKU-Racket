#lang racket

(define (main-loop)
  (define (tree-reverse lst)
    (if (list? lst) (reverse (map tree-reverse lst)) lst))
  (let ((input (read)))
    (if (eq? eof input)
        (void)
        (begin (displayln (tree-reverse input))
               (main-loop)))))

(main-loop)