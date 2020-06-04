#lang racket
;basic scheme interpreter ,with "let" , "and" and  "or" sentences  implemented.
;this program support this:
;(cond ((and (> 5 3) (> 6 2) (* 3 4))))
;=> 12


(require racket/mpair)
(define (mylist->mlist lst) 
  (if (null? lst)
      '()
      (if (pair? lst)
          (let ((first (car lst)))
            (if (or (mpair? first)  (pair? first))
                (mcons (mylist->mlist first)
                  (mylist->mlist (cdr lst)))
                (mcons first (mylist->mlist (cdr lst)))))
          (let ((first (mcar lst)))
            (if (or (mpair? first)  (pair? first))
                (mcons (mylist->mlist first)
                  (mylist->mlist (mcdr lst)))
                (mcons first (mylist->mlist (mcdr lst))))))))

(define (mymlist->list mlst)
  (if (null? mlst)
      '()
      (if (mpair? mlst)
          (let ((first (mcar mlst)))
            (if (or (mpair? first)  (pair? first))
                (cons (mymlist->list first)
                  (mymlist->list (mcdr mlst)))
                (cons first (mymlist->list (mcdr mlst)))))
          (let ((first (car mlst)))
            (if (or (mpair? first)  (pair? first))
                (cons (mymlist->list first)
                  (mymlist->list (cdr mlst)))
                (cons first (mymlist->list (cdr mlst))))))))
              
(define mcadr (lambda (x) (mcar (mcdr x))))
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)

(define (self-evaluating? exp)
   (cond ((number? exp) true)
         ((string? exp) true)
         (else false)))
(define (mtagged-list? exp tag) 

  (if (mpair? exp) 

      (eq? (mcar exp) tag)
      false))


(define (tagged-list? exp tag) 

  (if (pair? exp) 

      (eq? (car exp) tag)
      false))


(define (variable? exp) (symbol? exp))  

(define (quoted? exp)  (tagged-list? exp 'quote))

(define (text-of-quotation exp)  (cadr exp))

(define (assignment? exp)  (tagged-list? exp 'set!))

(define (assignment-variable exp)  (cadr exp))

(define (assignment-value exp)  (caddr exp))

(define (definition? exp)  (tagged-list? exp 'define))



(define (let? exp)  (tagged-list? exp 'let))
(define (let-body exp)  (cddr exp)) 

(define (let-clauses exp)  (cadr exp))
(define (let->combination exp)
  (cons (make-lambda (map car (let-clauses exp)) 

               (let-body exp)) (map cadr (let-clauses exp))))

(define (definition-variable exp)
  (if (variable? (cadr exp)) 

             (cadr exp)
             (caadr exp))) 


(define (definition-value exp) 
  (if (symbol? (cadr exp)) 
      (caddr exp) 
      (make-lambda (cdadr exp)             
                   (cddr exp)))) 


(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) 
   (cons 'lambda (cons parameters body)))  


(define (if? exp) (tagged-list? exp 'if)) 
(define (if-predicate exp) (cadr exp)) 

(define (if-consequent exp) (caddr exp)) 

(define (if-alternative exp)
  (if (null? (cdddr exp))
             'false
             (cadddr exp))) 

(define (make-if predicate consequent alternative)
  (if (null? consequent)
      (list 'if predicate predicate alternative)
      (list 'if predicate consequent alternative)))


(define (begin? exp)
  (tagged-list? exp 'begin))


(define (begin-actions exp) (cdr exp)) 

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (make-begin seq)
  (cons 'begin seq)) 

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq)) 
        (else (make-begin seq))))


(define (application? exp) (pair? exp))  
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops)) 

(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp)) 

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause)) 

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (eval exp env)
  (cond ((self-evaluating? exp ) exp)
        ((null? exp) (void))
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and (cdr exp) env))
        ((or? exp) (eval-or (cdr exp) env))
        ((map? exp) (eval-map (cdr exp) env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) 
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        
        ((cond? exp)  (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((application? exp)
         (my-apply (eval (operator exp) env)  

                (list-of-values (operands exp) env))) 

        (else
         (error "unknown expression type -- EVAL" exp))))


(define (my-apply procedure arguments)  
  (cond ((primitive-procedure? procedure)
         (apply (primitive-implementation procedure) arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment  

           (procedure-parameters procedure)
           (list->mlist arguments) 

           (procedure-enviroment procedure))))
        (else
         (error "unkonwn procedure type -- APPLY" procedure))))


(define (list-of-values exps env)  
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and operands env)
  (define (helper result ops env)
    (if (null? ops)
      result
      (let ((tmp (eval (car ops) env)))
        (if (true? tmp)
          (helper tmp (cdr ops) env)
          false))))
  (helper true operands env))


(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or operands env)
  (if (null? operands)
      false
      (if (true? (eval (car operands) env))
      true
      (eval-or (cdr operands) env))))

(define (map? exp)
  (tagged-list? exp 'map))

(define (eval-map exp env)
  (define (iter opt lst)
    (if (null? lst)
        (list)
        (let ((x (car lst))
              (xs (cdr lst)))
          (cons (my-apply opt (list x)) (iter opt xs)))))
  (let ((opt (eval (car exp) env))
        (lst (eval (cadr exp) env)))
    (iter opt lst)))
      
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps )env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
 (void))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env) 
    env)  
    (void))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))  

(define (compound-procedure? p)  
  (tagged-list? p 'procedure))

(define (procedure-parameters p) 
  (list->mlist (cadr p))) 


(define (procedure-body p)
  (caddr p))

(define (procedure-enviroment p)
  (cadddr p))

(define (enclosing-environment env) (mcdr env)) 
(define (first-frame env) (mcar env)) 
(define the-empty-environment (mlist )) 
(define (make-frame variables values) 
  (mcons variables values)) 


(define (frame-variables frame ) (mcar frame)) 

(define (frame-values frame) (mcdr frame))  


(define (add-binding-to-frame! var val frame)  
  (set-car! frame (mcons var (mcar frame)))  
  (set-cdr! frame (mcons val (mcdr frame))))  


(define (extend-environment vars vals base-env)  
  (if (= (mlength vars) (mlength vals))
      (begin  
      (mcons (make-frame vars vals) base-env)) 
       (if (< (length vars) (length vals))
           (error "Too many arguments supplied" vars vals)
           (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars)) 
             (mcar vals)) 
            (else (scan (mcdr vars) (mcdr vals))))) 
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))



(define (set-variable-value! var val env) 
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))  

             (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))  
    (if (eq? env the-empty-environment)
        (error "Unbound variable --SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env) 
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars)) 

             (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals))))) 
    (scan (frame-variables frame)  
          (frame-values frame))))


(define (my-square x ) (* x x))

(define primitive-procedures
  (mlist (mlist 'car car) 
        (mlist 'cdr cdr) 
        (mlist 'cons cons) 
        (mlist 'null? null?) 
        (mlist '+ +) 
        (mlist '* *) 
        (mlist '- -) 
        (mlist '/ /) 
        (mlist '< <) 
        (mlist '> >) 
        (mlist '= =) 
        (mlist 'number? number?) 
        (mlist 'pair? pair?) 
        (mlist 'not not) 
        (mlist 'remainder remainder) 
        (mlist 'my-square  my-square)
        (mlist 'length  length)
        (mlist 'sqrt  sqrt)
        (mlist 'list  list)
        (mlist 'symbol? symbol?)
        (mlist 'eq? eq?)
        (mlist 'cadr cadr)
        (mlist 'append append)
        )) 

(define primitive-procedures2
  (mlist
        (mlist '* *) 
        )) 

(define (primitive-procedure-names) 
  (mmap mcar  
       primitive-procedures))  

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (mcadr proc))) 
       primitive-procedures))


(define (setup-environment ) 
  (let ((initial-env
         (extend-environment (primitive-procedure-names) 
                             (primitive-procedure-objects)
                             the-empty-environment))) 
        (define-variable! 'true true initial-env)
        (define-variable! 'false false initial-env)
        initial-env))


        
(define (primitive-procedure? proc)  
        (mtagged-list? proc 'primitive)) 


(define (primitive-implementation proc) (mcadr proc)) 

(define (driver-loop)
  (let ((input (read))) 
    (if (eq? input eof)
        (void)
        (let ((output (eval input glb-env)))
          (user-print output)
          (driver-loop)))))


             
(define (user-print object)
   (if (compound-procedure? object)
       (display (list 'compound-procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      '<procedure-env>))
       (if (eq? object (void))
           object
           (begin (display object)(newline)))))

(define glb-env (setup-environment))
(driver-loop)


