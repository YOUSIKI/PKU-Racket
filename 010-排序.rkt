#lang racket

(define (read-integers)
  (let ((input (read)))
    (if (eq? eof input)
        (list)
        (cons input (read-integers)))))

(for-each
 (lambda (x) (display x) (display " "))
 (remove-duplicates (sort (read-integers) <)))