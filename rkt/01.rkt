#lang eopl

(define cumplen
  (lambda (pareja predicado)
    (and (predicado (car pareja)) (predicado (cadr pareja)))
    )
  )

(define cambiar
  (lambda (pareja)
    (cons (cadr pareja)
          (cons (car pareja) '()))))

(define invert
  (lambda (L P)
    (cond
      [(eqv? L empty) empty]
      [(cumplen (car L) P)
       (cons (cambiar (car L)) (invert (cdr L) P))]
      [else (invert (cdr L) P)]
      )
    )
  )