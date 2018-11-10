#lang racket
(require "spec.rkt")
(require "lambda-eval.rkt")

(define zero '(λ s (λ z z)))
(define one '(λ s (λ z (s z))))
(define two '(λ s (λ z (s (s z)))))
(define three '(λ s (λ z (s (s (s z))))))
(define four '(λ s (λ z (s (s (s (s z)))))))

;; Successor Function
(define S '(λ w (λ y (λ x (y ((w y) x))))))
(assert "Successor zero" (run one) (run (compose S zero)))
(assert "Successor one" (run two) (run (compose S one)))
(assert "Successors of zero to four" (run four) (run (compose S (compose S (compose S (compose S zero))))))

;; Addition:
(define ADD '(λ m (λ n (λ f (λ x ((m f) ((n f) x)))))))
(assert "Addition 1 + 1" (run two) (run (compose ADD one one)))
(assert "Addition 1 + 2 = 2 + 1"
        (run (compose ADD one two))
        (run (compose ADD two one)))

;; Convert between different representations of numbers
(define (number->church n)
  (define (iter i)
    (if (= i n)
        zero
        (compose S (iter (+ i 1)))))
  (run (iter 0)))

(assert "0 -> λ0" zero (number->church 0))
(assert "1 -> λ1" one (number->church 1))
(assert "2 -> λ2" two (number->church 2))

(define (church->number c)
  (define add1 (lambda (n) (+ n 1)))
  (((run-in-scheme c) add1) 0))

(assert "λ0 -> 0" 0 (church->number zero))
(assert "λ1 -> 1" 1 (church->number one))
(assert "λ2 -> 2" 2 (church->number two))

(assert "20 -> λ20 -> 20" 20 (church->number (number->church 20)))

(define MULT '(λ a (λ b (λ c (a (b c))))))
(assert "2*2=4" four (run (compose MULT two two)))

(provide zero)

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
(define ZERO? `(λ n ,(compose 'x F (compose NOT F))))
(assert "Is Zero" T (run (compose ZERO? zero)))
(assert "Is not Zero" F (run (compose ZERO? one)))