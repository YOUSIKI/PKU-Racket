#lang racket

(define (generate-huffman-tree nodes)
  (define (weight-cmp na nb)
    (< (weight na) (weight nb)))
  (define (generate-huffman-tree-iter pts)
    (if (= 1 (length pts)) (car pts)
        (let ((sorted-pts (sort pts weight-cmp)))
          (let ((pa (car sorted-pts))
                (pb (cadr sorted-pts))
                (ps (cddr sorted-pts)))
            (generate-huffman-tree-iter (cons (make-code-tree pa pb) ps))))))
  (let ((leafs (map (lambda (i) (make-leaf (car i) (cadr i))) nodes)))
    (generate-huffman-tree-iter leafs)))

(define (encode s tree)
  (define (encode-char c tr cur)
    (if (leaf? tr) cur
        (let ((lb (left-branch tr))
              (rb (right-branch tr)))
          (if (member c (map symbol->string (symbols lb)))
              (encode-char c lb (cons 0 cur))
              (encode-char c rb (cons 1 cur))))))
  (define (encode-iter ss cur)
    (if (null? ss) cur
        (encode-iter (cdr ss)
                     (cons (reverse (encode-char (car ss) tree (list))) cur))))
  (let ((ss (map string (string->list (symbol->string s)))))
    (flatten (reverse (encode-iter ss (list))))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left 
        right
        (append (symbols left ) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols  tree)
  (if (leaf? tree) 
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree) (weight-leaf tree)
      (cadddr tree)))
  
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit " bit))))


(define (adjoin-set x set)
;  (display "in adjoin-set:" ) (display "x=") (display x) (display "  set=" ) (display set) (newline);addfor debug
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
;  (display "in make-leaf-set:" ) (display pairs) (newline) ;addfor debug
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;(define (my-number->list num)
;  (if (< num 10)
;      (cons num '())
;      (append (my-number->list (floor (/ num 10))) (list (remainder num 10)))))
;(define tmp '((A 10000000) (B 1000000) (C 100000) (D 10000) (E 1000) (F 100) (G 10) (H 1)))
;(define tmptmp (make-leaf-set tmp))
;(define mytree (generate-huffman-tree tmp))
;(encode 'ABEFG mytree)


(define huffman-tree '())
(define (myloop) 
  (define (display-list lst)
    (if (null? lst)
        (void)
        (begin (display (car lst)) (display-list (cdr lst)))))

  (let ((a (read)))
     (if (eq? a eof)
         (void)
         (cond ((eq? a 'B) 
                (set! huffman-tree (generate-huffman-tree (read))) (myloop))
               ((eq? a 'E) 
                (display-list (decode (encode (read) huffman-tree) huffman-tree))
                (newline)
                (myloop))))))

(myloop)