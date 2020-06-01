#lang racket

(require scheme/mpair)

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      (void))    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define conversion-table (make-table))
(define get-coercion (conversion-table 'lookup-proc))
(define put-coercion (conversion-table 'insert-proc!))

;------------- integer package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))    
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) ((get 'make 'rational )  x y)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (void))

(define (make-integer n)
  ((get 'make 'integer) n))


;--------general functions
  
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= 2 (length args))
              (let ((t1 (car type-tags))
                    (t2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion t1 t2))
                      (t2->t1 (get-coercion t2 t1)))
                  (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                        (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                        (else (error "no method for " (list op args))))))
              (error "no method for " (list op args)))))))

(define (integer? x)
  (and (pair? x) (not (null? x)) (eq? 'integer (car x))))

(define (polynomial? x)
  (and (list? x) (not (null? x)) (eq? 'polynomial (car x))))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (display-poly p)
  (let ((poly->list (get-coercion 'polynomial 'list)))
    (displayln (poly->list p))))

(define (build-poly l)
  (define (iter i) (make-term (car i) (build-poly (cadr i))))
  (if (list? l)
      (make-poly
       (car l)
       (map iter (cdr l)))
      (make-integer l)))

(define (install-polynomial-package)
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? a b) (and (variable? a) (variable? b) (eq? a b)))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (=zero? term)
    (cond
      ;((integer? term) (=zero? (contents term)))
      ((integer? term) #f)
      ((polynomial? term) #f)
      (else (= 0 term))))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (integer->polynomial i) (make-polynomial 'x (list (make-term 0 i))))
  (define (polynomial->list p)
    (if (integer? p)
        (contents p)
        (cons (car (contents p))
              (map
               (lambda (item) (list (car item) (polynomial->list (cadr item))))
               (cdr (contents p))))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1) (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1) (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
        (let ((unified-list (unify-variable p1 p2)))
          (let ((up1 (car unified-list))
                (up2 (cadr unified-list)))
            (add-poly up1 up2)))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
        (let ((unified-list (unify-variable p1 p2)))
          (let ((up1 (car unified-list))
                (up2 (cadr unified-list)))
            (mul-poly up1 up2)))))
  (define (unify-variable p1 p2)
    (let ((v1 (variable p1))
          (v2 (variable p2)))
      (cond
        ((same-variable? p1 p2) (list v1 v2))
        ((eq? 'x v1) (list (make-poly v2 (term-list p1)) p2))
        ((eq? 'x v2) (list p1 (make-poly v1 (term-list p2))))
        (else
         (let ((vp1 (variable-priority v1))
               (vp2 (variable-priority v2)))
           (if (< vp1 vp2)
               (list p1 (make-poly v1 (list (make-term 0 (tag p2)))))
               (list (make-poly v2 (list (make-term 0 (tag p1)))) p2)))))))
  (define (variable-priority v)
    (cond ((eq? 'a v) 1)
          ((eq? 'b v) 2)
          ((eq? 'c v) 3)
          ((eq? 'd v) 4)
          ((eq? 'e v) 5)
          (else (error "no priority for variable " v))))
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  (put 'make 'polynomial-term make-term)
  (put-coercion 'integer 'polynomial integer->polynomial)
  (put-coercion 'polynomial 'list polynomial->list)
  (void))

(install-integer-package)
(install-polynomial-package)
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))


(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))
(define (make-term order coeff) 
  ((get 'make 'polynomial-term) order coeff))

(displayln "******1")
(define e1 (make-poly 'a (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3a+2
(define e2 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4a^2 + 3a
(displayln e1)
(displayln e2)
(displayln (add e1 e2))
(displayln (mul e1 e2))

(displayln "******2")

(define c1 (make-poly 'b (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3b+2
(define c2 (make-poly 'b (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4b^2 + 3b

(define e3 (make-poly 'a (list (list 1 c1) (list 0 (make-integer 2))))) 
(define e4 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 c2)))) 

(displayln (add e3 e4))

(displayln "******")
(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (let ((op (car a))
              (e1 (cadr a))
              (e2 (caddr a)))
          (if (eq? op '+)
              (display-poly (add (build-poly e1) (build-poly e2)))
              (display-poly (mul (build-poly e1) (build-poly e2))))
          (myloop)))))
              
(myloop)