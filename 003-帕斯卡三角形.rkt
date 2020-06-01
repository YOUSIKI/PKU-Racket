#lang racket

(define (next-line line)
  (define (next-line-iter cur rest)
    (if (= 1 (length rest))
        (cons 1 cur)
        (next-line-iter
         (cons (+ (car rest)
                  (cadr rest))
               cur)
         (cdr rest))))
  (reverse (next-line-iter (list 1) line)))

(define (main-loop)
  (define (case-loop n line)
    (if (= 0 n)
        (void)
        (begin
          (for-each
           (lambda (x) (display x) (display " "))
           line)
          (newline)
          (case-loop (- n 1) (next-line line)))))
  (let ((input (read)))
    (if (eq? eof input)
        (void)
        (begin (case-loop input (list 1))
               (main-loop)))))

(main-loop)