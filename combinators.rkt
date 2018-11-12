#lang racket
(require "spec.rkt")
(require "fix.rkt")
(require "numerals.rkt")

(define S '(λ x (λ y (λ z ((x z) (y z))))))
(define K '(λ x (λ y x)))
(define I (run (compose S K K)))

(define T K)
(define F (run (compose S K)))

(define zero (compose S K))

(define (make-lambda arg body)
  (list 'λ arg body))

(define (lambda? exp)
  (and (pair? exp)
       (eq? 'λ (car exp))))

(define (proc-formal-arg exp)
  (cadr exp))

(define (proc-body exp)
  (caddr exp))

(define (application? exp)
  (and (not (lambda? exp))
       (pair? exp)))

(define (variable? exp)
  (symbol? exp))

(define (reducible? exp comp)
  (and (lambda? exp)
       (alpha-reduce? exp comp)))

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

(define (SK-var? exp)
  (not (eq? #f (memq exp '(S K I)))))

(define (simplify-SK exp)
  (cond ((SK-var? exp) exp)
        ((eq? (car exp) 'I)
         (simplify-SK (cadr exp)))
        ((eq? (car exp) 'K) exp)
        ((eq? (caar exp) 'K)
         (simplify-SK (cadar exp)))
        ((or (eq? (car exp) 'S)
             (eq? (caar exp) 'S)) exp)
        ((eq? (caaar exp) 'S)
         `((,(cadaar exp) ,(simplify-SK (cadr exp))) (,(simplify-SK (cadar exp)) ,(simplify-SK (cadr exp)))))
        (else "Error faild -- SIMPLIFY" exp)))

;; From: https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis
(define (λ->SK exp)
  (cond ((variable? exp) exp)
        ((application? exp) `(,(λ->SK (car exp)) ,(λ->SK (cadr exp))))
        ((not (free? (proc-formal-arg exp) (proc-body exp)))
         `(K ,(λ->SK (proc-body exp))))
        ((reducible? exp I) 'I)
        ((and (lambda? exp)
              (lambda? (proc-body exp))
              (free? (proc-formal-arg exp)
                     (proc-body (proc-body exp))))
         (λ->SK
          (make-lambda
           (proc-formal-arg exp)
           (λ->SK
            (make-lambda
             (proc-formal-arg
              (proc-body exp))
             (proc-body
              (proc-body
               exp)))))))
        (else `((S
                 ,(λ->SK
                   (make-lambda
                    (proc-formal-arg exp)
                    (car (proc-body exp)))))
                ,(λ->SK
                  (make-lambda
                   (proc-formal-arg exp)
                   (cadr (proc-body exp))))))))

(define (run-SK exp)
  (if (variable? exp)
      exp
      `(compose ,(run-SK (car exp))
                ,(run-SK (cadr exp)))))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(define (SK->λ exp)
  (run (eval (run-SK exp) ns)))