#lang racket
(define exit 5)
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your my-map-ex, the program in the input can use my-map-ex
 '
(define (my-map-ex proc . args)
  (define (all-check x)
    (if (null? x) #t (and (car x) (all-check (cdr x)))))
  (define (helper lists)
    (let ((cars (map car lists))
          (cdrs (map (lambda (l) (if (null? (cdr l)) l (cdr l))) lists)))
      (let ((cur (apply proc cars)))
        (if (all-check (map (lambda (l) (null? (cdr l))) lists))
            (list cur)
            (cons cur (helper cdrs))))))
  (if (null? args) (list) (helper args)))
env)

(define (myloop)
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin  (displayln (eval codes env)) (myloop)))))


(myloop)