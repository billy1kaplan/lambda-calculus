#lang racket
(require "spec.rkt")
(require "lambda-eval.rkt")
(require "numerals.rkt")

(define T '(λ x (λ y x)))
(define F '(λ x (λ y y)))

;; Define AND
(define ^ `(λ w (λ z ((w z) ,F))))
(assert "And T T" T (run (compose ^ T T)))
(assert "And T F" F (run (compose ^ T F)))
(assert "And F T" F (run (compose ^ F T)))

;; Define OR
(define OR `(λ w (λ z ((w ,T) z))))
(assert "Or T T" T (run (compose OR T T)))
(assert "Or T T" T (run (compose OR T T)))

;; Define NOT
(define NOT `(λ w ((w ,F) ,T)))
(assert "Not F" T (run (compose NOT F)))
(assert "Not T" F (run (compose NOT T)))

;; Define ZERO?
(define ZERO? `(λ x ,(compose 'x F NOT F)))
(assert "Is Zero" T (run (compose ZERO? zero)))