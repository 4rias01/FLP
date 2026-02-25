#lang eopl

;Taller FLP: Punto 10*

;Funciones Auxiliares--------------------------------------------------------------

;; auxParenth 
;; Proposito :
;; L x N -> Bool : Este procedimiento recibe una lista de simbolos L de dos tipos
;; 'O (open) y 'C (close) asi como una variable entera de acumulacion N. Retorna un Booleano que 
;; sera #true si los parentesis estan balanceados (en ningun momento hay mas C que O
;; en la lista), y #false en el caso contrario o en que los datos de entrada no sean correctos.
;;
;; <List> ::= ()
;;        ::= (<Symbol> <List>)

(define auxParenth
  (lambda (list acum)
    (cond
      ( (< acum 0) #f ) 
      ( (null? list) (= acum 0) ) 
      ( (eqv? (car list) 'O ) (auxParenth (cdr list) (+ acum 1)))
      ( (eqv? (car list) 'C ) (auxParenth (cdr list) (- acum 1)))    
      (else #f))))

;; Pruebas:
;; Caso General
(auxParenth '(O C O O C C) 0) ;; Retorno => #t
;; Caso Base incorrecto/procesado
(auxParenth '() 2) ;; Retorno => #f
;; Caso acumulador negativo
(auxParenth '(C O) 0) ;; Retorno => #f
;; Caso secuencia valida pero acumulador inicializado
(auxParenth '(O C) 1) ;; Retorno => #f

;Fin Funciones Auxiliares----------------------------------------------------------


;; balanced-parentheses?
;; Proposito :
;; L -> Bool : Este procedimiento recibe una lista de simbolos de tipo 'O y 'C, y empleando
;; la funcion auxiliar auxParenth (debido a que no podemos modificar los parametros de entrada
;; y si o si necesitamos un acumulador), la cual nos retorna el valor Booleano.
;;
;; <List> ::= ()
;;        ::= (<Symbol> <List>)

(define balanced-parentheses?
  (lambda (L)
    (auxParenth L 0)
    ))


;; Pruebas :