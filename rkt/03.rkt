#lang eopl

(define list-set
  (lambda (L n x P)
    (cond
      [(eqv? L empty) empty]
      [(= n 0)
       (if (P (car L))
               (cons x (cdr L))
               L)]
      [else
       (cons (car L)
             (list-set (cdr L) (- n 1) x P))])
    )
  )