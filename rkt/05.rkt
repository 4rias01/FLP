#lang eopl

(define unir-listas
  (lambda (L1 L2)
    (cond
      [(eqv? L1 empty) L2]
      [(number? L1) (cons L1 L2)]
      [else (cons (car L1) (unir-listas (cdr L1) L2))]))
  )

(define invertir-lista
  (lambda (L)
    (cond
      [(eqv? L empty) empty]
      [else (unir-listas (invertir-lista (cdr L)) (list (car L)))]
      ))
  )

(define palindrome?
  (lambda (palabra)
    (let
        (
         (p-inversa (invertir-lista palabra))
         )
      (equal? palabra p-inversa)
      )
    )
  )