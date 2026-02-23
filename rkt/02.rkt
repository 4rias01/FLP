#lang eopl

(define down
  (lambda (L)
    (cond
      [(eqv? L empty) empty]
      [else (cons (cons (car L) '()) (down (cdr L)))]
      )
    )
  )