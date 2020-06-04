#lang racket

(require r5rs)
(define (exit) #f)
(define env (scheme-report-environment 5))

(eval '(define (last-pair lst)
         (if (null? (cdr lst))
             lst
             (last-pair (cdr lst))))
      env)

(eval '(define (make-cycle lst)
         (set-cdr! (last-pair lst) lst)
         lst)
      env)

(eval '
      (define (check-cycle lst)
        (define (dfs u)
          (if (pair? u)
              (let ((ul (car u))
                    (ur (cdr u)))
                (if (eq? 'VISITING ul)
                    #t
                    (begin (set-car! u 'VISITING)
                           (dfs ur))))
              #f))
        (dfs lst))
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