#lang eopl

;Taller FLP: Punto 12*

;Funciones Auxiliares--------------------------------------------------------------



;Fin Funciones Auxiliares----------------------------------------------------------

;; Doble condicional : si esta en el intervalo y SI cumple con filter 
;; parametro inclusivo

(define filter-acum
  (lambda (a b F acum filter)
    (if (> a b)
        acum
        (if (equal? (filter a) #t)
            (filter-acum (+ a 1) b F (+ acum (F a)) filter)
            (filter-acum (+ a 1) b F (+ acum 0) filter) ))))




;; Pruebas :