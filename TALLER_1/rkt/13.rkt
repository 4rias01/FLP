#lang eopl

;Taller FLP: Punto 12*

;Funciones Auxiliares--------------------------------------------------------------



;Fin Funciones Auxiliares----------------------------------------------------------


(define ultimo
  (lambda (lista)
    (cond
      [(null? (cdr lista)) (car lista)]
      [else (ultimo (cdr lista))])))

(define lst-sin-ultimo
  (lambda (lista)
    (cond
      [(eqv? (cdr lista) empty) empty]
      [else (cons (car lista) (lst-sin-ultimo (cdr lista)))])
    )
  )

(define operate
  (lambda (lrators lrands)
    (cond
      [(null? (cddr lrands)) ((car lrators) (car lrands) (cadr lrands))]
      [else ((ultimo lrators) (operate (lst-sin-ultimo lrators) (lst-sin-ultimo lrands)) (ultimo lrands))])
    )
  )

;; Pruebas :