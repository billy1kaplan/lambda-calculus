#lang racket
(require srfi/13)

;; Redefining to remember how these work...
(define (foldl-p f list)
  (if (null? list)
      '()
      (foldl f (car list) (cdr list))))

(define (list->string l)
  (cond ((not (pair? l)) (symbol->string l))
        ((lambda? l) (string-append "(λ " (list->string (cadr l)) " " (list->string (caddr l)) ")"))
        (else
         (string-append "(" (list->string (car l)) " " (list->string (cadr l)) ")"))))

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

(define (push stack item)
  (cons (list 0 item) stack))

(define (push-n stack n item)
  (cons (list n item) stack))

(define (replace stack spaces match)
  (map (lambda (item)
         (if (equal? (cadr item) match)
             (list spaces (cadr item))
             item))
       stack))

(define (pop stack)
  (cdr stack))

(define (peek stack)
  (cadr stack))

(define (depth stack)
  (if (null? stack)
      0
      (caar stack)))

;; Force: This gives a fixed-point simplification (which may or may not exist)
(define (force exp)
  (let ((next (cond ((variable? (car exp)) exp)
                    ((lambda? (car exp)) (force-lambda exp))
                    ((application? (car exp)) (eval-application exp))
                    (else
                     (error "Unknown expression type -- FORCE" exp)))))
    (if (equal? (car exp) (car next))
        exp
        (force next))))

(define (force-lambda exp)
;  (cons (make-proc (proc-formal-arg (car exp))
;                   (car (force (cons (proc-body (car exp))
 ;                                    (push (cdr exp) (car exp))))))
  ;      (cdr exp)))
  (cons (make-proc (proc-formal-arg (car exp))
                   (car (force (cons (proc-body (car exp))
                                     (cdr exp)))))
        (cdr exp)))

;; Evaluator:
(define (lambda-eval exp)
  (cond ((not (pair? (car exp))) exp)
        ((lambda? (car exp)) exp) ;; Don't evaluate lambdas until applied
        ((application? (car exp)) (eval-application exp))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (eval-application exp)
  (let ((f (car (lambda-eval (cons (car (car exp)) (cdr exp)))))
        (env (cdr (lambda-eval (cons (car (car exp)) (cdr exp)))))
        (arg (cadr (car exp))))
    (if (proc? f)
        (let ((substitution-result (substitute (proc-formal-arg f) arg (proc-body f) env)))
          (lambda-eval substitution-result))
        ; (let ((result (lambda-eval (cons arg (push (cdr exp)
        ;                                           (list f arg))))))
        (let ((result (lambda-eval (cons arg (cdr exp)))))
          (cons (list f (car result))
                (cdr result))))))

(define (substitute var val exp prev)
  (define (internal exp)
    (cond ((not (free? var exp)) exp)
                 ((variable? exp) val)
                 ((lambda? exp) (list 'λ (cadr exp) (internal (caddr exp))))
                 (else
                  (list (internal (car exp))
                        (internal (cadr exp))))))
  (define (find history)
    (let ((location (string-contains (list->string (cadar prev)) (list->string exp))))
      (if location
          (cons (+ location (caar prev)) (cadar prev))
          (find (cdr history)))))
    (define (parent history)
    (let ((location (string-contains (list->string (cadar prev)) (list->string exp))))
      (if location
          (caar prev)
          (find (cdr history)))))
  (let* ((result (internal exp))
        (location (find prev))
        (parents (make-space (parent prev)))
        (spaces (make-space (car location))))
    (begin
      (display (string-append parents (list->string (cdr location))))
      (newline)
      (display (string-append spaces
               (list->string exp)
               " where "
               (symbol->string var)
               " is "
               (list->string val)))
    (newline)
    (display (string-append (make-space (car location))
                            (list->string result)))
    (newline)
    (cons result
          (push-n prev (car location) result)))))

(define (make-space n)
  (if (= n 0)
      ""
      (string-append " " (make-space (- n 1)))))

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
    (define (simplify-lambda exp env)
      (let ((replacement-var (next-variable)))
        (make-proc replacement-var
                   (simplify (substitute (cadr exp)
                                         replacement-var
                                         (proc-body exp))))))
    (define (simplify exp env)
      (cond ((not (pair? exp)) exp)
            ((lambda? exp) (simplify-lambda exp env))
            ((application? exp) (cons (simplify (car exp) env)
                                      (simplify (cadr exp) env)))
            (else
             (error "Unhandled case! -- SIMPLIFY" exp))))
     (simplify exp)))

;; Do the expressions reduce to the same generic form?
(define (alpha-reduce? exp1 exp2)
  (equal? (genericize "x" exp1)
          (genericize "x" exp2)))

;; Runs a lambda calculus expression by reducing it as far as possible
(define (explain exp)
   (car (force (cons exp (push '() exp)))))

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

(provide explain)
