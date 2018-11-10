#lang racket/gui

(require racket/gui/base)

(define application-frame
  (new frame%
       [label "Test"]
       [width 400]
       [height 300]))

(define main-panel (new group-box-panel%
                        (parent application-frame)
                        (label "Main computation")
                        (vert-margin 10)))

(define (make-computation-step)  (new panel%
                                    (parent main-panel)
                                    (style (list 'border))))

(send application-frame show #t)

(define (message component text)
  (new message%
       (parent component)
       (label text)))

(let ((start "(λ x x)")
      (first-step (make-computation-step)))
    (message first-step start))