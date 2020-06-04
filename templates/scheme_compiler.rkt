#lang racket
(require racket/mpair)

;;;;;;;;;;;;;;;;;;;;;****common functions
(define (exit) (void))
(define (mtagged-list? exp tag) 
  (if (mpair? exp) 
      (eq? (mcar exp) tag)
      false))

(define (tagged-list? exp tag) 
  (if (pair? exp) 
      (eq? (car exp) tag)
      false))

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
;;;;;;;;;;;;;;;;;;;;;;program of basic scheme interpreter
;functions below needs to be add to the scheme machine's op list, not in basic scheme interpreter
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))


(define (self-evaluating? exp)
   (cond ((number? exp) true)
         ((string? exp) true)
         (else false)))


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

(define (procedure-environment p)
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


(define (primitive-procedure-names) 
  (mmap mcar  
       primitive-procedures))  

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (mcadr proc))) 
       primitive-procedures))

(define (extend-environment vars vals base-env)  
  (if (not (mpair? vals))
      (set! vals (list->mlist vals))
      (void))  ;addfor scheme-machine by guo wei
  (if (not (mpair? vars))
      (set! vars (list->mlist vars))
      (void))  ;addfor scheme-machine by guo wei ;add for compiler

  (if (= (mlength vars) (mlength vals))
      (begin  
      (mcons (make-frame vars vals) base-env)) 
       (if (< (length vars) (length vals))
           (error "Too many arguments supplied" vars vals)
           (error "Too few arguments supplied" vars vals))))


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

(define glb-env (setup-environment))


;;;;;;;;;;;;;;;;;;;;;;;;;****basic machine interpreter 

(define (eof? x)
  (if (eq? x eof)
      true
      false))  ;add by guo wei

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))
(define (get-global-environment)
  glb-env)

(define (user-print x)
  (if (eq? x (void))
      (void)
      (displayln x)))

(define (apply-primitive-procedure op args)
  (apply (primitive-implementation op) args))



(define eceval-operations 
           (list
                (list 'rem remainder)                                  
                (list 'self-evaluating? self-evaluating?)
		(list 'variable? variable?)
		(list 'quoted? quoted?)
		(list 'assignment? assignment?)
		(list 'definition? definition?)
		(list 'if? if?)
		(list 'lambda? lambda?)
		(list 'begin? begin?)
		(list 'application? application?)
		(list 'lookup-variable-value lookup-variable-value)
		(list 'text-of-quotation text-of-quotation)
		(list 'lambda-parameters lambda-parameters)
		(list 'lambda-body lambda-body)
		(list 'make-procedure make-procedure)
		(list 'operands operands)
		(list 'operator operator)
		(list 'empty-arglist empty-arglist)
		(list 'no-operands? no-operands?)
		(list 'first-operand first-operand)
		(list 'last-operand? last-operand?)
		(list 'adjoin-arg adjoin-arg)
		(list 'rest-operands rest-operands)
		(list 'primitive-procedure? primitive-procedure?)
		(list 'compound-procedure? compound-procedure?)
		(list 'apply-primitive-procedure apply-primitive-procedure)
		(list 'procedure-parameters procedure-parameters)
		(list 'procedure-environment procedure-environment)
		(list 'procedure-body procedure-body)
		(list 'extend-environment extend-environment)
		(list 'begin-actions begin-actions)
		(list 'first-exp first-exp)
		(list 'last-exp? last-exp?)
		(list 'rest-exps rest-exps)
		(list 'no-more-exps? no-more-exps?)
		(list 'if-predicate if-predicate)
		(list 'true? true?)
		(list 'if-alternative if-alternative)
		(list 'if-consequent if-consequent)
		(list 'assignment-variable assignment-variable)
		(list 'assignment-value assignment-value)
		(list 'set-variable-value! set-variable-value!)
		(list 'definition-variable definition-variable)
		(list 'definition-value definition-value)
		(list 'define-variable! define-variable!)
                ;(list 'prompt-for-input prompt-for-input)
		(list 'read read)
		(list 'get-global-environment get-global-environment)
		;(list 'announce-output announce-output)
		(list 'user-print user-print)
                (list 'eof? eof?) ;add by guo wei
                )
  )



(define (make-register name) 
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      (void))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))


(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))


(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  (void))

(define (get-register machine reg-name)  
  ((machine 'get-register) reg-name))



(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine) 
      insts)))


(define (extract-labels text receive) 
  
  (if (null? text)
      (receive '() '()) 
      
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
               (receive (mcons (make-instruction next-inst)  

                              insts)
                        labels)))))))


(define (update-insts! insts labels machine) 

  

  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations))) 

    (mfor-each 

     (lambda (inst)
       (set-instruction-execution-proc! 
        inst  

        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts))) 





(define (make-instruction text) 

  (mcons text '())) 
(define (instruction-text inst)
  (mcar inst))
(define (instruction-execution-proc inst)
  (mcdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-mcdr! inst proc)) 





(define (make-label-entry label-name insts)
  (cons label-name insts)) 
 

 


(define (lookup-label labels label-name)   

  (let ((val (assoc label-name labels)))
    (if val
        (cdr val) 

        (error "Undefined label -- ASSEMBLE" label-name))))






(define (make-execution-procedure inst labels machine pc flag stack ops)
  
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (begin (displayln (car inst))(error "Unknown instruction type -- ASSEMBLE"
                     inst) ))))


(define (make-assign inst machine labels operations pc)
  (let ((target 
         (get-register machine (assign-reg-name inst)))  
        (value-exp (assign-value-exp inst))) 
    (let ((value-proc
           (if (operation-exp? value-exp)  
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))  
      (lambda ()                
        (set-contents! target (value-proc)) 
        (advance-pc pc))))) 



(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))


(define (advance-pc pc) 

  (set-contents! pc (mcdr (get-contents pc))))   



(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst))) 

    (if (operation-exp? condition) 

        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))



(define (make-branch inst machine labels flag pc) 
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))


(define (make-goto inst machine labels pc) 

  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))



(define (make-save inst machine stack pc) 
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))


(define (make-primitive-exp exp machine labels) 
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () 
             (if (eq? c 'ok)
                 (void)
                 c))))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels  

                              (label-exp-label exp))))
           (lambda () insts))) 

        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))  

        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))


(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))


(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))  
        (aprocs
         (map (lambda (e)  

                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()  
         (apply op (map (lambda (p) (p)) aprocs)))))




(define (operation-exp? exp) 
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))


(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))
(define (make-new-machine) 
  (let ((pc (make-register 'pc)) 
        (flag (make-register 'flag)) 
        (stack (make-stack))
        (the-instruction-sequence '())) 
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag)))) 
      (define (allocate-register name) 
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name) 
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)  

              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))  

          (if (null? insts)
              (void)
              (begin
                ((instruction-execution-proc (mcar insts))) 

                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence) 

               (execute)) 

              ((eq? message 'install-instruction-sequence) 
               (lambda (seq) 
                 (set! the-instruction-sequence seq)
                 ))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations) 

               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack) 

              ((eq? message 'operations) the-ops) 
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))



(define (make-machine register-names ops controller-text);
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (start machine)
  (machine 'start))

;;;;your code starts here
(define extra-ops(list 
                (list 'make-compiled-procedure make-compiled-procedure)
                (list 'compiled-procedure? compiled-procedure?)
                (list 'compiled-procedure-entry compiled-procedure-entry)
                (list 'compiled-procedure-env compiled-procedure-env)
                (list 'list list) ;no say to add in text book
                (list 'cons cons) ;no say to add in text book
                (list 'false? false?)))

(set! eceval-operations (append eceval-operations extra-ops))


;;;;;;;;;;;;;;;;;;;;;;;;;;****scheme machine controler
(define scheme-machine-controller 
'(

  (branch (label external-entry))  
; 新scheme机器起点 ,branches if flag is set
  
read-eval-print-loop
  (perform (op initialize-stack))
  ;(perform
   ;(op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))  ;before doing somthing that may change the return address, always assign continue with right label
  (goto (label eval-dispatch))
print-result
;  (perform
;   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val)) ;the value of exp is stored in val
  (goto (label read-eval-print-loop))

external-entry
  (perform (op initialize-stack))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (reg val))


  
  
eval-dispatch
;after this is completed, the value of exp is stored in reg val,and 
;program goto the address stored in reg continue;
; eval value of exp in env
  (test (op eof?) (reg exp)) ;addby guo wei
  (branch (label program-end))
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))  ;assign exp to val and then goto continue
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))
  
ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))
ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure)
              (reg unev) (reg exp) (reg env))
  (goto (reg continue))
;just store the value of exp in reg val,and goto continue
;the value of a lambda is a function object, and it is stored also in val  

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)                   ; save variable for later
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))  ; evaluate the assignment value stored in exp
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op set-variable-value!) (reg unev) (reg val) (reg env)) ;variable name is stored in uenv
  (assign val (const ok))
  (goto (reg continue))
  
 
  
  
ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)                   ; save variable for later
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))  ; evaluate the definition value
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
  
  
ev-if
  (save exp)                    ; save the whole if expression for later
  (save env)
  (save continue) ;after ev-if,should goto continue
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch))  ; evaluate the predicate  
  
  
ev-if-decide
  (restore continue) ;the address to which we should go after the whole ev-if is done
  (restore env)  ;the env for the whole if exp
  (restore exp)  ;the whole if exp
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))

ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))  

  

ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))
  
ev-sequence
  (test (op no-more-exps?) (reg unev))
  (branch (label ev-sequence-end))
  (assign exp (op first-exp) (reg unev))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-end
  (restore continue)
  (goto (reg continue))  
  
  
  
  
ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))


ev-appl-did-operator
  (restore unev)                  ; the operands
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))         ; the operator,is a function object
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)


ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev) ;the oprands, in which the first one is going to be evaluated and can be discarded
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))

ev-appl-accumulate-arg
  (restore unev)  ;operands, in which the fisrt one is already evaluated
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))

ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))



;modifor compiler
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
  (test (op compiled-procedure?) (reg proc))  
  (branch (label compiled-apply))
  (goto (label unknown-procedure-type))
compiled-apply
  (restore continue) ;编译代码执行前要求返回地址在continue
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))



primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))




compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
              (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))
                   
  
unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))
unknown-procedure-type
  (restore continue)    ; clean up stack (from apply-dispatch)
  (assign val (const unknown-procedure-type-error))
  (goto (label signal-error))
signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))
program-end ;addfor scheme-machine by guo wei
)
)  ;end of scheme-machine-controller
  
;;;;;;;;;;;;;;;;;;;;;compiler below
(define all-regs '(exp env val proc argl continue unev))
;functions about instruction sequences
(define (list-union s1 s2)
  ;merge s1 & s2 and remove the repeated elements
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))
(define (list-difference s1 s2)
 ;remove elements which are in s2 from s1
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))



(define (registers-needed s) (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))
(define (needs-register? seq reg) ;seq是指令序列
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))
(define (empty-instruction-sequence) ;空的指令序列
  (make-instruction-sequence '() '() '()))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2) 
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs) 
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs)) ;seqs中的元素也可以是标号,如'somewhere


(define (preserving regs seq1 seq2) ;regs是可能需要保存到栈里的寄存器集合
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))  
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))


(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))
(define (compile-lambda-body exp proc-entry)
;proc-entry是个标号
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl) '(env)
;函数体的代码要求执行前env放着环境,proc放着译后过程,argl放着实参值表
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
       (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))

     (compile-sequence (lambda-body exp) 'val 'return))))
;整个程序中唯一使用 'return 作为linkage参数的地方
;函数体执行结束一定是 (goto (reg continue)) 

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
;整个程序中,唯一 target不是 val 的地方。proc用来放“译后过程” 
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env continue);求值运算符会改变env和continue
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))
;construct-arglist 生成构造实参表的代码
;compile-procedure-call 生成调用过程的代码

(define (construct-arglist operand-codes)
;operand-codes是个列表,每个元素都是对一个参数求值的指令序列
  (let ((operand-codes (reverse operand-codes))) ;倒过来
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
;生成求剩余参数值的指令
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes) ;求出一个参数的值并放入 val 的指令
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl
              (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  ;仅被compile-application调用
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage ;使得调用compile-proc-appl时,linkage不可能为'next
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences ;不用考虑寄存器出入栈的指令序列拼接
       (make-instruction-sequence '(proc) '()
                                  `((test (op primitive-procedure?) (reg proc))
                                    (branch (label ,primitive-branch))))
       (parallel-instruction-sequences ;两个指令序列拼接，只会执行一个
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
;compile-proc-appl生成调用复合函数的指令。用compiled-linkage而非linkage是因为如果linkage为'next，则调用复合函数的代码执行完后，应该跳过调用基本函数的代码，转到after-call.
       
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence
           '(proc argl)
           (list target)
           `((assign ,target  
                    (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

(define (compile-proc-appl target linkage); linkage不可能是'next
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs ;所有寄存器都可能修改
           `((assign continue (label ,linkage)) 
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
                                    
        ((and (not (eq? target 'val)) ;此时target是 'proc
              (not (eq? linkage 'return))) ; linkage是 label
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
              `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                          (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage)))
            )))

        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry)
                        (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))



(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue) 
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq) target linkage))))

(define (compile-lambda exp target linkage);exp是lambda表达式
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target 
                    (op make-compiled-procedure) ; 译后过程
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))


(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage ; end-with-linkage 处理后续如何执行(连接代码)
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))))
(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,(text-of-quotation exp))))))) 
(define (compile-variable exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '(env) (list target)
    `((assign ,target 
              (op lookup-variable-value)
              (const ,exp) ; 形如 (const x)
              (reg env))))))

(define (end-with-linkage linkage instruction-sequence)
;返回一个指令序列
  (preserving '(continue) 
;需要关注coutinue是否可能被instruction-sequence修改且在第二个指令序列中用到
   instruction-sequence
   (compile-linkage linkage)))
(define (compile-linkage linkage) ;生成链接后续执行代码的指令列表
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage))))))) ;eeee


(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next)))
; 变量值求出后放入val,接下来用val值对变量名赋值,因此linkage是 'next
    (end-with-linkage linkage
     (preserving '(env) ;env可能在get-value-code中被修改。为何没 val?
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!) ;eeee
                  (const ,var) ;变量名
                  (reg val) ;变量值
                  (reg env)) ;变量所在环境
         (assign ,target (const ok))))))))


(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op set-variable-value!) ;eeee
                  (const ,var) ;变量名
                  (reg val) ;变量值
                  (reg env)) ;变量所在环境
         (assign ,target (const ok))))))))


(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch)) ;make-label生成无重复标号
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch)))) ;eeee
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))



(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))
;;;;;your code ends here

(define scheme-machine
  (make-machine
   '(exp env val proc argl continue unev c d)
   eceval-operations
   scheme-machine-controller
  ))

(define (compile-and-go expression)
  (let ((instructions
         (assemble (statements  ;assemble由文本形式的指令列表生成scheme机器的指令列表(带可执行过程的指令的列表,该可执行过程是scheme可执行过程)
                    (compile expression 'val 'return))
                   scheme-machine))) ;eceval是个scheme机器
    (set-register-contents! scheme-machine 'val instructions)
    (set-register-contents! scheme-machine 'flag true)
    (start scheme-machine)))
;val放着编译出来的代码的起始地址
;return保证编译出来的代码最后是 (goto (reg continue))
(compile-and-go (read))
;输入:

;part A: 一个 define 表达式，定义了一个scheme函数

;part B:
;一段scheme程序，可能调用了part A中的函数

;要求输出part B的执行结果
