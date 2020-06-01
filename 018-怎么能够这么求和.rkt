#lang racket

(define (mysum x)
  (define (iter . args)
    (if (null? args) x (mysum (+ x (car args)))))
  iter)
  
(define (f s n)
  (define (exec g i) ; call g for i times
    (if (= i 1)
        (g)
        (begin (g) (exec g (- i 1)))))
    
  (define (iter k ls)
    (if (null? ls)
        (exec k n) ;call k for n times. same effect as (k). just to prevent cheating
        (iter (k (car ls)) (cdr ls))))
  (iter (mysum (car s)) (cdr s)))

(define k ((((mysum 1) 2) 3) 4))
(define k2 ((mysum 10) 20))

(displayln (k))
(displayln (k2))

(define (myloop)
  (let ((s (read)))
    (if (eq? s eof)
        (void)
        (begin (displayln (f s (car s)))
               (myloop)))))
(myloop)