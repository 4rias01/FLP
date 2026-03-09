#lang eopl

;Taller FLP: Punto 8*

;; mapping :
;; Proposito :
;; F x L x L -> LP : Este procedimiento recibe una funcion unaria sobre numeros F y dos listas de 
;; numeros L de igual tamaño. Esta funcion retorna una lista de pares (a,b) LP donde
;; el primer elemento pertenece a la primer lista, y el otro termino a la segunda,
;; en estos pares se CUMPLE la condicion de que F(a) = b.
;; 
;; <List> ::= ()
;;        ::= (<number> <List>)
;; 
;; <Pair> ::= (<number> <number>)
;;
;; <Pair-List> ::= ()
;;             ::= (<Pair> <Pair-List>)

(define mapping
  (lambda (F L1 L2)
    (if (null? L1)
        '()
        (if (null? L2)
            '()
            (if (= (F (car L1)) (car L2))
                (cons  (cons (car L1)(car L2)) (mapping F (cdr L1) (cdr L2))  )
                (mapping F (cdr L1) (cdr L2) ) )))))

;; Pruebas: