#lang racket
(require "spec.rkt")
(require "lambda-eval.rkt")
(require "explain.rkt")

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
(define ZERO? `(λ n ,(compose 'n F NOT F)))
(assert "Is Zero" T (run (compose ZERO? zero)))
(assert "Is not Zero" F (run `(,ZERO? ,one)))

;; Define Predecessor
(define pred '(λ p (λ f (λ x (((p (λ g (λ h (h (g f))))) (λ u x)) (λ u u))))))
(assert "Predeccessor of zero" zero (run `(,pred ,zero)))
(assert "Predecessor of one" zero (run `(,pred ,one)))
(assert "Predecessor of two" one (run `(,pred ,two)))

;; Greater or equal?
(define GT= `(λ x (λ y ,(compose ZERO? (compose 'x pred 'y)))))
(assert "3 >= 2" T (run (compose GT= three two)))
(assert "3 >= 3" T (run (compose GT= three three)))
(assert "2 >= 3" F (run (compose GT= two three)))

;; Greater than?
(define GT `(λ x (λ y ,(compose ZERO? (compose 'x pred (compose S 'y))))))
(assert "3 > 2" T (run (compose GT three two)))
(assert "3 > 3" F (run (compose GT three three)))
(assert "2 > 3" F (run (compose GT two three)))

(define ZERO-OR-ONE? `(λ o (,ZERO? (,pred o))))
(assert "0 <= 1" T (run `(,ZERO-OR-ONE? ,zero)))
(assert "1 <= 1" T (run `(,ZERO-OR-ONE? ,one)))
(assert "2 <= 1" F (run `(,ZERO-OR-ONE? ,two)))

(define Y '(λ f ((λ x (f (λ y ((x x) y))))
                 (λ x (f (λ y ((x x) y)))))))

(define almost-factorial
  `(λ f
    (λ n
      (((,ZERO? n)
        ,one)
       ((,MULT n) (f (,pred n)))))))

(define factorial (compose Y almost-factorial))
(assert "4!" (number->church 24) (run (compose factorial four)))

(define almost-fib
  `(λ f
     (λ n
       (((,ZERO-OR-ONE? n)
         n)
        ((,ADD (f (,pred n))) (f (,pred (,pred n))))))))

(define fib (compose Y almost-fib))
(assert "fib 6" (number->church 8) (run (compose fib (number->church 6))))
                    
(provide zero)
(provide one)
(provide S)
(provide Y)
(provide fib)
(provide number->church)
(provide church->number)
