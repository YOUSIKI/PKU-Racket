#lang racket

(define (cont-frac-iter N D k)
  (define (iter k current N D)
    (if (= k 0) current (iter (- k 1) (/ (N k) (+ (D k) current)) N D)))
  (iter k 0 N D))
  
(cont-frac-iter (lambda (x) x) 
           (lambda (x) 1.0)
           30)
 
(cont-frac-iter (lambda (x) (* 2 x))
           (lambda (x) (* 1.0 x))
           30)

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) k)) 
(newline) (myloop)))))

(myloop)