#lang racket

(define (exit) #f)
(define (map op lst)
  (if (null? lst)
      '()
      (cons (op (car lst))
            (map op (cdr lst)))))
  
(define (super-map op . w)
  (define (zip . lists)
    (if (or (null? lists) (null? (car lists)))
        (list)
        (let ((cars (map car lists))
              (cdrs (map cdr lists)))
          (cons cars (apply zip cdrs)))))
  (map (lambda (lst) (foldr op 0 lst)) (apply zip w)))
  
(define (myloop)
  (let ((a (read))
        (b (read))
        (c (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (super-map + a b c)) 
               (displayln (super-map (lambda (x y) (+ x (* 2 y) )) a b ))
               (myloop)))))
(myloop)