#lang racket

(define (merge segments)
  (define (merge-iter segs ans)
    (cond ((null? segs) ans)
          ((null? ans) (merge-iter (cdr segs) (list (car segs))))
          (else (let ((first-seg (car segs)) (first-ans (car ans)))
                  (let ((ls (car first-seg)) (rs (cadr first-seg)) (la (car first-ans)) (ra (cadr first-ans)))
                    (if (> ls ra)
                        (merge-iter (cdr segs) (cons first-seg ans))
                        (merge-iter (cdr segs) (cons (list la (max ra rs)) (cdr ans)))))))))
  (merge-iter segments (list)))

(define cmp (lambda (a b) (< (car a) (car b))))

(define (main-loop)
  (let ((a (read))
        (b (read)))
    (if (eq? eof a)
        (void)
        (begin
          (displayln (reverse (merge (sort (append a b) cmp))))
          (main-loop)))))

(main-loop)