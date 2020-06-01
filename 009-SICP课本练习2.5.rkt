#lang racket

(define (car p)
  (define (car-iter num cnt)
    (if (= 0 (modulo num 2))
        (car-iter (/ num 2) (+ cnt 1))
        cnt))
  (car-iter p 0))

(define (cdr p)
  (define (cdr-iter num cnt)
    (if (= 0 (modulo num 3))
        (cdr-iter (/ num 3) (+ cnt 1))
        cnt))
  (cdr-iter p 0))

(define (fast-exp a n)
  (define (square x) (* x x))
  (define (iter a n result)
    (if (= n 0)
        result
        (if (even? n) 
            (iter (square a) (/ n 2) result)
            (iter (square a) (/ (- n 1) 2) (* a result)))))
  (iter a n 1))
  
(define (cons a b)
  (* (fast-exp 2 a) (fast-exp 3 b)))

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (display (car (cons a b)))
               (display " ")
               (display (cdr (cons a b)))
               (newline) 
               (myloop)))))

(myloop)