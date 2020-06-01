#lang racket

(define (main-loop cases)
  (define (case-loop n ints)
    (if (= 0 n)
        (displayln (apply max ints))
        (case-loop (- n 1) (cons (read) ints))))
  (if (= 0 cases)
      (void)
      (begin (case-loop (read) (list))
             (main-loop (- cases 1)))))

(main-loop (read))