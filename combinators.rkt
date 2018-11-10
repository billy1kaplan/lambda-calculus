#lang racket
(require "spec.rkt")
(require "lambda-eval.rkt")

(define S '(λ x (λ y (λ z ((x z) (y z))))))
(define K '(λ x (λ y x)))
(define I (run (compose S K K)))

;(define S '(λ w (λ y (λ x (y ((w y) x))))))

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

;; From: https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis
(define (convert-lambda exp)
  (display exp)
  (display (and (lambda? exp)
       (lambda? (proc-body exp))
       (free? (proc-formal-arg exp)
              (proc-body (proc-body exp)))))
  (display (and (lambda? exp)
                (not (free? (proc-formal-arg exp) (proc-body exp)))))
  (display (variable? exp))
  (newline)
  (cond ((variable? exp) exp)
        ((application? exp) `(,(convert-lambda (car exp))
                              ,(convert-lambda (cadr exp))))
        ((not (free? (proc-formal-arg exp) (proc-body exp)))
         `(K ,(convert-lambda (proc-body exp))))
        ((reducible? exp I) 'I)
        ((and (lambda? exp)
              (lambda? (proc-body exp))
              (free? (proc-formal-arg exp)
                     (proc-body (proc-body exp))))
         (convert-lambda
          (make-lambda
           (proc-formal-arg exp)
           (convert-lambda
            (make-lambda
             (proc-formal-arg
              (proc-body exp))
             (proc-body
              (proc-body
               exp)))))))
        (else `((S
                 ,(convert-lambda
                   (make-lambda
                    (proc-formal-arg exp)
                    (car (proc-body exp)))))
                ,(convert-lambda
                  (make-lambda
                   (proc-formal-arg exp)
                   (cadr (proc-body exp))))))))
(convert-lambda K)