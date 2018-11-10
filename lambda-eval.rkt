#lang racket

;; Redefining to remember how these work...
(define (foldr f init xs)
  (if (null? xs)
      init
      (f (car xs)
         (foldr f init (cdr xs)))))

(define (foldl f init xs)
  (define (internal acc xs)
    (if (null? xs)
        acc
        (internal
         (f (car xs) acc)
         (cdr xs))))
  (internal init xs))

(define (foldl-p f list)
  (if (null? list)
      '()
      (foldl f (car list) (cdr list))))

;; Selectors:
(define (make-proc arg body)
  (list 'λ arg body))

(define (proc? exp)
  (and (pair? exp)
       (eq? 'λ (car exp))))

(define (proc-formal-arg exp)
  (cadr exp))

(define (proc-body exp)
  (caddr exp))

(define (λ-arg exp)
  (cadr exp))

;; Frames:
(define (lookup var env)
  (let ((result (assoc var env)))
    (if result
        (cdr result)
        var)))

(define (make-empty-env) '())

(define (extend-environment var val env)
  (cons (cons var val)
        env))

;; Evaluator Selector:
(define (variable? exp) (and (not (pair? exp))
                             (symbol? exp)))
(define (lambda? exp)
  (and (pair? exp)
       (eq? 'λ (car exp))))

(define (application? exp) (pair? exp))

;; Evaluator:
(define (lambda-eval exp env)
  (cond ((null? exp) exp)
        ((number? exp) exp)
        ((variable? exp) (lookup exp env))
        ((lambda? exp) (eval-lambda exp env))
        ((application? exp) (eval-application exp env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define empty-symbol (string->symbol ""))
(define (empty? exp)
  (equal? empty-symbol exp))

(define (eval-lambda exp env)
  (let ((arg (proc-formal-arg exp))
        (body (lambda-eval (proc-body exp) env)))
    (make-proc arg body)))

(define (eval-application exp env)
  (let ((f (lambda-eval (car exp) env))
        (arg (lambda-eval (cadr exp) env)))
    (if (proc? f)
        (begin
          ;(display exp)
          ;(newline)
          ;(display `(,(proc-formal-arg f) is ,arg in ,(proc-body f)))
          ;(newline)
          (let ((result
                 (lambda-eval (proc-body f)
                       (extend-environment (proc-formal-arg f) arg env))))
           ; (display result)
           ; (newline)
            result))
        (list f arg))))

;; Replace variables with generic names in a deterministic fashion
(define (genericize var-prefix exp)
   (let ((count 0))
    (define (next-variable)
      (set! count (+ count 1))
      (string->symbol (string-append var-prefix
                                     (number->string count))))
    (define (lookup exp env)
      (let ((result (assoc exp env)))
        (if result
            (cdr result)
            (next-variable))))
    (define (simplify-lambda exp env)
      (let ((replacement-var (next-variable)))
        (make-proc replacement-var (simplify
                                    (proc-body exp)
                                    (extend-environment (proc-formal-arg exp)
                                                        replacement-var
                                                        env)))))
    (define (simplify exp env)
      (cond ((null? exp) exp)
            ((number? exp) exp)
            ((variable? exp) (lookup exp env))
            ((lambda? exp) (simplify-lambda exp env))
            ((application? exp) (cons (simplify (car exp) env)
                                      (simplify (cdr exp) env)))
            (else
             (error "Unhandled case! -- SIMPLIFY" exp))))
     (simplify exp (make-empty-env))))

;; Do the expressions reduce to the same generic form?
(define (alpha-reduce? exp1 exp2)
  (equal? (genericize "x" exp1) (genericize "x" exp2)))

;; Runs a lambda calculus expression by reducing it as far as possible
(define (run exp)
  (lambda-eval exp (make-empty-env)))

;; Composes a list of left-associative functions
;(define (compose . fs)
; (foldl-p (lambda (cur acc) `(,acc ,cur)) fs))

;; Resolves name conflicts by uniquely naming variables in each lambda function
(define (compose . fs)
  (let ((count 0))
    (foldl-p (lambda (cur acc)
               (begin (set! count (+ count 1))
                      `(,acc ,(genericize (string-append "x" (number->string count)) cur))))
             fs)))

;; Transforms (λ x x) -> (λ (x) x) for compatibility with the real lisp evaluator
(define (make-compat exp)
  (cond ((lambda? exp) (make-proc (list (proc-formal-arg exp))
                                  (make-compat (proc-body exp))))
        ((pair? exp) (cons (make-compat (car exp))
                           (make-compat (cdr exp))))
        (else exp)))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(define (run-in-scheme exp)
  (eval (make-compat exp) ns))

(provide run)
(provide compose)
(provide alpha-reduce?)
(provide make-compat)
(provide run-in-scheme)
(provide genericize)