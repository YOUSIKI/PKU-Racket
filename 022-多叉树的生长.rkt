#lang racket

(define (create-tree n h temp)
  (define (create-tree-helper rest-h first-idx)
    (if (= 0 rest-h)
        (list first-idx)
        (cons first-idx
              (map (lambda (i)
                     (create-tree-helper
                      (- rest-h 1)
                      (+ i (* first-idx n))))
                   temp))))
  (create-tree-helper h 0))

(define (main-loop)
  (let ((temp (read)) (n (read)) (h (read)))
    (if (eq? eof temp)
        (void)
        (begin (displayln (create-tree n h temp))
               (main-loop)))))

(main-loop)