#lang eopl

(define invert
  (lambda (L P)
    (cond
      ((eqv? L empty) empty)
      ((and (P (caar L)) (P (cadr (car L)))) (cons (car L) (invert (cdr L) P)))
      (else invert (cdr L) P)
      )
    )
  )