#lang eopl

(define swapper
  (lambda (E1 E2 L)
    (cond
      [(eqv? L empty) empty]
      [(eqv? (car L) E1) (cons E2
                               (swapper E1 E2 (cdr L)))]
      [(eqv? (car L) E2) (cons E1
                               (swapper E1 E2 (cdr L)))]
      [else (cons (car L)
                  (swapper E1 E2 (cdr L))
                  )])
    )
  )