#lang racket
(require r5rs)
(define env (scheme-report-environment 5))

(define (check-share lst)
  (let ((tag (random)))
    ;(displayln tag)
    (define (helper cur)
      ;(displayln cur)
      (cond ((null? cur) #f)
            ((pair? cur)
             (if (helper (car cur)) #t
                 (if (not (pair? (car cur)))
                     (begin (set-car! cur tag)
                            (helper (cdr cur)))
                     (if (helper (cdr cur)) #t
                         (if (not (pair? (cdr cur)))
                             (begin (set-cdr! cur tag)
                                    #f)
                             #f)))))
            (else (eq? cur tag))))
    (if (helper lst)
        (displayln "True")
        (displayln "False"))))
  
(displayln "**********")
(define lst1 (list 'a 'b))
(define lst2 (list 5))
(define pair1 (cons 1 12))
(define pair2 (cons 'cc 3))
(check-share lst1)
(check-share pair1)



(displayln "**********")
(check-share (cons lst1 lst1))
(check-share (list pair1 pair2))
(check-share (list (cons lst2 pair1) lst2))
(check-share (list (list pair2 (cons 1 12) pair1) (append lst1 lst2)))
(displayln "**********")



(define (myloop)
  (define declaration
    '((define lst1 (list 'a 'b))
      (define lst2 (list 5))
      (define pair1 (cons 1 12))
      (define pair2 (cons 'cc 3))))
  
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin
          (check-share
           (eval-codes (append declaration codes) (void)))
          (myloop)))))


(myloop)