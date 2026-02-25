#lang eopl

;Taller FLP: Punto 10*

;Funciones Auxiliares--------------------------------------------------------------



;Fin Funciones Auxiliares----------------------------------------------------------
(define auxParenth
  (lambda (list acum)
    (cond
      ( (< acum 0) #f ) 
      ( (null? list) (= acum 0) ) 
      ( (eqv? (car list) 'O ) (auxParenth (cdr list) (+ acum 1)))
      ( (eqv? (car list) 'C ) (auxParenth (cdr list) (- acum 1)))    
      (else #f))))

(define balanced-parentheses?
  (lambda (L)
    (auxParenth L 0)
    ))


;; Pruebas :