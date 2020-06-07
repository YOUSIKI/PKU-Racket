#lang racket
(require r5rs)
(define env (scheme-report-environment 5))
(define exit 5)


(eval '(define (power-sum
                . nums)
         (let ((s (apply + nums)))
           (define (iter . args)
             (if (null? args) s (power-sum (+ s (apply + args)))))
           iter))
      env)
                
(define (myloop)
  (let ((exp (read)))
    (if (eq? eof exp)
        (void)
        (let ((val (eval exp env)))
          (if (eq? val (void))
              (begin (void) (myloop))
              (begin (displayln val) (myloop)))))))
(myloop)