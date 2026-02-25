#lang eopl

(define unir-listas
  (lambda (L1 L2)
    (cond
      [(eqv? L1 empty) L2]
      [else (cons (car L1) (unir-listas (cdr L1) L2))]))
  )

(define sumar-listas
  (lambda (L1 L2)
    (cond
      [(or (eqv? L1 empty) (eqv? L2 empty)) empty]
      [else (cons (+ (car L1) (car L2)) (sumar-listas (cdr L1) (cdr L2)))]))
  )

(define pascal
  (lambda (N)
    (if (= N 1)
        '(1)
        (letrec ([fila-prev (pascal (- N 1))]
                 [fila1 (cons 0 fila-prev)]
                 [fila2 (unir-listas fila-prev '(0))])
          (sumar-listas fila1 fila2)))))




