#lang racket

(define (repeated f n)
  (if (= 1 n) f (compose f (repeated f (- n 1)))))

(define (cmp a b)
  (cond ((not (or (list? a) (list? b))) (< a b))
        ((null? a) false)
        ((null? b) true)
        (else (let ((car-a (car a))
                    (car-b (car b))
                    (cdr-a (cdr a))
                    (cdr-b (cdr b)))
                (if (eq? car-a car-b)
                    (cmp cdr-a cdr-b)
                    (cmp car-a car-b))))))

(define flt (compose remove-duplicates (lambda (lst) (sort lst cmp))))

(define (powerset s)
  (if (null? s) (list (list))
      (let ((first (car s))
            (rests (cdr s)))
        (let ((rests-powerset (powerset rests)))
          (flt (append rests-powerset
                       (map (lambda (a) (cons first a)) rests-powerset)))))))

(define (main-loop)
  (let ((s (read))
        (n (read)))
    (if (eq? eof s)
        (void)
        (begin (displayln ((repeated powerset n) (flt s)))
               (main-loop)))))

(main-loop)
