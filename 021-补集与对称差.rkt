#lang racket

(define (relative-complement a b)
  (sort (remove-duplicates (filter (lambda (x) (false? (member x b))) a)) <))

(define (symmetric-difference a b)
  (sort (remove-duplicates (append (relative-complement a b) (relative-complement b a))) <))

(define (main-loop)
  (let ((a (read))
        (b (read)))
    (if (eq? eof a)
        (void)
        (begin
          (display (relative-complement a b))
          (display (symmetric-difference a b))
          (newline)
          (main-loop)))))

(main-loop)