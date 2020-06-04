#lang racket

(require r5rs)
(define (exit) #f)
(define env (scheme-report-environment 5))

(eval '(define (acc-func . argf)
         (define (iter fs x)
           (if (null? fs) x (iter (cdr fs) ((car fs) x))))
         (let ((funcs argf))
           (define (ret-func . args)
             (if (and (= 1 (length args)) (number? (car args)))
                 (iter funcs (car args))
                 (begin (set! funcs (append funcs args))
                        ret-func)))
           ret-func))
env)


(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))
(myloop)