#lang racket
;; message : Failure Message for failed assertion
;; expected : Expected value for the assertion
;; actual : The actual value provided
(require "lambda-eval.rkt")

(define (assert message expected actual)
  (if (alpha-reduce? expected actual)
      (display ".")
      (fprintf (current-output-port)
               "
               Failed: ~a
               Expected: ~a
               Received: ~a"
               message
               expected
               actual)))

(provide assert)
