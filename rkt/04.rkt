#lang eopl

(define filter-in
  (lambda (P L)
    (cond
      [(eqv? L empty) empty]
      [else (if (P (car L))
                (cons (car L) (filter-in P (cdr L)))
                (filter-in P (cdr L))
                )])
    )
  )