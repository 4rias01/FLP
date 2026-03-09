#lang eopl

;Taller FLP: Punto 7*

;Funciones Auxiliares--------------------------------------------------------------

;; auxCombi :
;; Proposito:
;; Sim x L -> L' : Procedimiento que construye una lista de pares L', combinando
;; el simbolo dado Sim con cada elemento de la lista de simbolos L.
;;
;; <List> ::= ()
;;        ::= (<Scheme-value> <List>)
;;
;; <Pair> ::= (<Scheme-value> <Scheme-value>)
;;
;; <Pair-List> ::= ()
;;             ::= (<Pair> <Pair-List>)

(define auxCombi
  (lambda (a L)
    (if (null? L)
        '()
        (cons (cons a (car L)) (auxCombi a (cdr L) ) ) )))

;; Pruebas
;; Caso General
(auxCombi 'a '(1 2 3)) ;; Retorno => ((a . 1) (a . 2) (a . 3))
;; Caso 2 simbolos
(auxCombi 'ty '(0 2 9 23)) ;; Retorno => ((ty . 0) (ty . 2) (ty . 9) (ty . 23))
;; Caso lista vacia
(auxCombi 'r '()) ;; Retorno => ()
;; Caso Simbolos Mixtos
(auxCombi '7m '(0 u 9 23)) ;; Retorno => ((7m . 0) (7m . u) (7m . 9) (7m . 23))






;; append :
;; Proposito :
;; L x L -> L' : Procedimiento que toma los elementos de dos listas de Simbolos L, y 
;; los junta en una nueva Lista L' que contiene todos los simbolos de la primer lista
;; seguidos de los de la segunda.
;;
;; <List> ::= ()
;;        ::= (<Scheme-value> <List>)

(define append
  (lambda (L1 L2)
    (if (null? L1)
        L2
        (cons (car L1) (append (cdr L1) L2) ) )))

;; Pruebas
;; Caso general listas mixtas
(append '(a b c) '(6 14 7 9)) ;; Retorno => (a b c 6 14 7 9)
;; Caso primer lista vacia
(append '() '(x y z)) ;; Retorno => (x y z)
;; Caso segunda lista vacia
(append '(1 2 3) '()) ;; Retorno => (1 2 3)
;; Caso ambas listas vacias
(append '() '()) ;; Retorno => ()

;Fin Funciones Auxiliares----------------------------------------------------------

;; cartesian-product :
;; Proposito :
;; L x L -> LP : Procedimiento que recibe 2 listas de simbolos sin repeticion L 
;; y retorna una nueva lista de tuplas LP que representan el producto 
;; cartesiano entre ambas listas.
;; 
;; <List> ::= ()
;;         ::= (<Scheme-value> <List>)
;;
;; <Pair> ::= (<Scheme-value> <Scheme-value>)
;;
;; <Pair-List> ::= ()
;;             ::= (<Pair> <Pair-List>)

(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1)
        '()
        (if (null? L2)
            '()
            (append (auxCombi (car L1) L2) (cartesian-product (cdr L1) L2) ) ))))

;;Pruebas
;;