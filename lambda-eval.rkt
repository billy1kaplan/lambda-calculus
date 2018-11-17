#lang racket
;; Redefining to remember how these work...
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

;; Evaluator Selector:
(define (variable? exp)
  (symbol? exp))

(define (lambda? exp)
  (and (pair? exp)
       (eq? 'λ (car exp))))

(define (application? exp)
  (and (not (lambda? exp))
       (pair? exp)))

;; Force: This gives a fixed-point simplification (which may or may not exist)
(define (force exp)
  (let ((next (cond ((not (pair? exp)) exp)
                    ((lambda? exp) (force-lambda exp))
                    ((application? exp) (eval-application exp))
                    (else
                     (error "Unknown expression type -- FORCE" exp)))))
    (if (equal? exp next)
        exp
        (force next))))

(define (force-lambda exp)
  (make-proc (proc-formal-arg exp)
             (force (proc-body exp))))

;; Evaluator:
(define (lambda-eval exp)
  (cond ((not (pair? exp)) exp)
        ((lambda? exp) exp) ;; Don't evaluate lambdas until applied
        ((application? exp) (eval-application exp))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (eval-application exp)
  (let ((f (lambda-eval (car exp)))
        (arg (cadr exp)))
    (if (proc? f)
        (lambda-eval (substitute (proc-formal-arg f) arg (proc-body f)))
        (list f (lambda-eval arg)))))

(define explain? #f)
(define explanation '())

(define (substitute var val exp)
  (define (internal exp)
    (cond ((not (free? var exp)) exp)
                 ((variable? exp) val)
                 ((lambda? exp) (list 'λ (cadr exp) (internal (caddr exp))))
                 (else
                  (list (internal (car exp))
                        (internal (cadr exp))))))
  (let ((result (internal exp)))
    (if explain?
        (set! explanation (mcons (list var val exp result) explanation))
        (void))
    result))

(define (explain exp)
  (define (reverse-m list)
    (define (iter acc list)
      (if (null? list)
          acc
          (iter (cons (mcar list) acc) (mcdr list))))
    (iter '() list))
  (set! explain? #t)
  (let ((result (force (run exp))))
    (let ((reasons (reverse-m explanation)))
      (set! explain? #f)
      (set! explanation '())
      (list exp reasons result))))

(define (pp explanation)
  (let ((exp (car explanation))
        (reasons (cadr explanation))
        (result (caddr explanation)))
    (display "Initial expression: ")
    (display exp)
    (newline)
    (display "Computation steps: ")
    (newline)
    (for-each (lambda (r) (display r) (newline)) reasons)
    (display "Result: ")
    (display result)
    (newline)))

;; Determines if a variable is free in a given expression
(define (free? var exp)
  (cond ((variable? exp) (eq? var exp))
        ((application? exp) (or (free? var (car exp))
                                (free? var (cadr exp))))
        ((lambda? exp)
         (if (eq? var (proc-formal-arg exp))
             #f
             (free? var (proc-body exp))))
        (else
         (error "Unmatched expression -- FREE?" exp))))

;; Replace variables with generic names in a deterministic fashion
(define (genericize var-prefix exp)
   (let ((count 0))
    (define (next-variable)
      (set! count (+ count 1))
      (string->symbol (string-append var-prefix
                                     (number->string count))))
    (define (simplify-lambda exp)
      (let ((replacement-var (next-variable)))
        (make-proc replacement-var
                   (simplify (substitute (cadr exp)
                                         replacement-var
                                         (proc-body exp))))))
    (define (simplify exp)
      (cond ((not (pair? exp)) exp)
            ((lambda? exp) (simplify-lambda exp))
            ((application? exp) (cons (simplify (car exp))
                                      (simplify (cadr exp))))
            (else
             (error "Unhandled case! -- SIMPLIFY" exp))))
     (simplify exp)))

;; Do the expressions reduce to the same generic form?
(define (alpha-reduce? exp1 exp2)
  (equal? (genericize "x" exp1)
          (genericize "x" exp2)))

;; Runs a lambda calculus expression by reducing it as far as possible
(define (run exp)
   (force exp))

;; Applies a list of functions associatively: (a b c) -> ((a b) c)
(define (compose . fs)
    (foldl-p (lambda (cur acc) `(,acc ,cur)) fs))

;; Transforms (λ x x) -> (λ (x) x) for compatibility with the real scheme evaluator
(define (make-compat exp)
  (cond ((lambda? exp) (make-proc (list (proc-formal-arg exp))
                                  (make-compat (proc-body exp))))
        ((pair? exp) (cons (make-compat (car exp))
                           (make-compat (cdr exp))))
        (else exp)))

;; Anchor to our current namespace
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
