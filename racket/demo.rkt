#lang racket
(require "explain.rkt")
(require "lambda-eval.rkt")
(require "logic.rkt")
(require "combinators.rkt")
(require "numerals.rkt")

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (demo code)
  (if (null? code)
      (void)
      (begin (display "> ") (display (car code))
             (read-line)
             (display (eval (car code) ns))
             (newline)
             (newline)
             (demo (cdr code)))))

(define code '(zero
               one
               (run (compose S zero))
               (explain (compose S zero))
               Y
               fib
               (number->church 10)
               (church->number (number->church 10))
               (run (compose fib (number->church 8)))
               (church->number (run (compose fib (number->church 8))))))

(define (fast code)
  (if (null? code)
      (void)
      (begin (display (eval (car code) ns))
             (newline)
             (fast (cdr code)))))

(define (go)
  (demo code))