#lang racket
;book ex 3.55
(require r5rs)
(define env (scheme-report-environment 5))
(eval '(define exit 5) env)
(eval '(define (stream-car stream) (car stream)) env)
(eval '(define (stream-cdr stream) (force (cdr stream))) env)
(eval '(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))])) env)

(eval '(define the-empty-stream '()) env)
(eval '(define (stream-null? stream) (null? stream)) env)
         
(eval '(define (stream-ref s n)  ;取 stream里面第 n 项,n从0开始算
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
      env)
(eval '(define (display-stream s n) ;display first n items of s
  (if (= n 0)
      (displayln "")
      (begin (display (stream-car s)) (display " ") (display-stream (stream-cdr s) (- n 1)))))

      env)

(eval '
 (define (partial-sums op s) ;以s为参数,返回的流是 (op s0),(op s0) (op s0 s1),(op s1 s0) (op s0 s1 s2),(op s2 s1 s0) ....
   (define (partial-sums-helper stream lst)
     (let ((s0 (stream-car stream))
           (ss (stream-cdr stream)))
       (let ((new-lst (append lst (list s0))))
         (let ((rev-lst (reverse new-lst)))
           (let ((a (apply op new-lst))
                 (b (apply op rev-lst)))
             (cons-stream a (cons-stream b (partial-sums-helper ss new-lst))))))))
   (partial-sums-helper s (list)))
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