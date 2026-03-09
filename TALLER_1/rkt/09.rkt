#lang eopl

;Taller FLP: Punto 9*

;Funciones Auxiliares--------------------------------------------------------------

;; sumAux
;; Proposito :
;; L -> N : Procedimiento que recibe una lista de numeros sin repeticion L, la cual se
;; recorre en su totalidad, comparando el primer termino (car L) con el resto de
;; elementos de la lista, sumando 1 cada vez que la cabeza sea mayor, indicando
;; que esta invertido. Esta retornara el numero total de inversiones N presentes
;; en la lista respecto al primer termino de L.
;;
;; <List> ::= ()
;;        ::= (<number> <List>)

(define sumAux
  (lambda(L)
    (if (null? L)
        0
        (if (null? (cdr L))
            0
            (if (> (car L) (car (cdr L)) )
                (+ 1 (sumAux (cons (car L) (cdr(cdr L)) ) ))
                (sumAux (cons (car L) (cdr(cdr L)) ) ) ) ) )))

;; Pruebas :
;; Caso general
(sumAux '(5 3 4 1)) ;; Retorno => 3
;; Caso un solo elemento
(sumAux '(5)) ;; Retorno => 0
;; Caso sin inversiones
(sumAux '(1 2 3 4)) ;; Retorno => 0
;; Caso lista vacia
(sumAux '()) ;; Retorno => 0

;Fin Funciones Auxiliares----------------------------------------------------------


;; inversions
;; Proposito :
;; L -> N : Este procedimiento recibe una lista de numeros sin repeticion L, la funcion
;; recorre toda la lista de manera recursiva, pasandole cada elemento de esta a la funcion
;; auxiliar sumAux, para finalmente retornar el numero total de inversiones N.
;;
;; <List> ::= ()
;;        ::= (<number> <List>)

(define inversions
  (lambda (L)
    (if (null? L)
        0
        (+ (sumAux L) (inversions (cdr L)) )  )))

;; Pruebas :