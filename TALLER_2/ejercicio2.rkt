#lang eopl

;; TALLER 2: EJERCICIO 2:
;; Santiago Arias Rojas (202416285)
;; Sebastian Calvo Carvajal (202419118)
;; Juan Jose Rodriguez Lozano (202419084)


;; terminal -> se lee directo
;; NO terminal -> Requiere su propio PARSER.
;;
;; Gramatica:
;;
;     <lit-exp>   ::=  <int>
;                 ::= -<int>

;     <or-exp>    ::= <lit-exp> or <list-lit>

;     <list-lit>  ::= <lit-exp>
;                 ::= (or-exp)

;     <clausula>  ::= (<list-lit>)

;     <and-exp>   ::= <clausula> and <list-clausula>

; <list-clausula> ::= <clausula>
;                 ::= <and-exp>

; 2. Implementacion por medio de LISTAS (Una forma diferente de representar la gramatica sin usar constructores y extractores a mano)
; 0. Gramatica
;     <lit-exp>   ::=  <int>
;                 ::= -<int>
;
;     <or-exp>    ::= <lit-exp> or <list-lit>
;
;     <list-lit>  ::= <lit-exp>
;                 ::= (or-exp)
;
;     <clausula>  ::= (<list-lit>)
;
;     <and-exp>   ::= <clausula> and <list-clausula>

; <list-clausula> ::= <clausula>
;                 ::= <and-exp>

; 1. Implementacion por medio de Constructores y Extractores


; 1. Implementacion por medio de Constructores y Extractores


; ---- lit-exp ----

; Constructor: make-pos-lit
;
; Crea un literal positivo a partir de un numro entero n.
; Se usa para representar variables comunes en la FNC.
;
; Pruebas:
;(make-pos-lit 3)  => (pos-lit 3)
;(make-pos-lit 1)  => (pos-lit 1)
(define (make-pos-lit n)
     (list 'pos-lit n)
)

; Constructor: make-neg-lit
;
; Crea un literal negativo a partir de un entero n
; Se usa para representar variables negadas en la FNC
;
; Pruebas:
; (make-neg-lit 3)  => (neg-lit 3)
; (make-neg-lit 7)  => (neg-lit 7)
(define (make-neg-lit n)
     (list 'neg-lit n)
)

; Extractor: lit-exp->n
;
; Extrae el entero n de un lit-exp (positivo o negativo)
; Se usa para obtener el valor numerico de un literal
; Es auxiliar de cualquier funcion que necesite operar sobre el valor del literal
;
; Pruebas:
; (lit-exp->n (make-pos-lit 5))  => 5
; (lit-exp->n (make-neg-lit 2))  => 2
(define lit-exp->n
     (lambda (lit-exp)
          (cadr lit-exp)
     )
)



; ---- list-lit ----

; Constructor: make-or-exp
;
; Crea una expresion OR entre un literal l y el resto de la lista de literales rest
; Se usa para encadenar literales dentro de una clausula
;
; Pruebas:
; (make-or-exp (make-pos-lit 1) (make-pos-lit 2))  => (or-exp (pos-lit 1) (pos-lit 2))
; (make-or-exp (make-neg-lit 3) (make-pos-lit 4))  => (or-exp (neg-lit 3) (pos-lit 4))
(define (make-or-exp l rest)
     (list 'or-exp l rest)
)

; Extractor: list-lit->l
;
; Extrae el primer literal l de una list-lit
; Se usa para acceder al primer elemento de una lista de literales
; Es auxiliar de funciones que recorren o procesan una list-lit
;
; Pruebas:
; (list-lit->l (make-or-exp (make-pos-lit 1) (make-pos-lit 2)))  => (pos-lit 1)
; (list-lit->l (make-pos-lit 5))  => 5  ; caso single-lit, devuelve el contenido directo
(define list-lit->l
     (lambda (list-lit)
          (cadr list-lit)
     )
)





; ---- or-exp ----

; Extractor: or-exp->l
;
; Extrae el literal izquierdo l de una or-exp.
; Se usa para obtener el primer operando de una expresion OR.
; Es auxiliar de funciones que procesan expresiones OR dentro de una clausula.
;
; Pruebas:
; (or-exp->l (make-or-exp (make-pos-lit 2) (make-neg-lit 3)))  => (pos-lit 2)
; (or-exp->l (make-or-exp (make-neg-lit 1) (make-pos-lit 4)))  => (neg-lit 1)
(define or-exp->l
   (lambda (or-exp)
      (cadr or-exp)
   )
)

; Extractor: or-exp->rest
;
; Extrae el resto de la lista de literales de una or-exp
; Se usa para continuar el recorrido de una expresion OR
; Es auxiliar de funciones que recorren expresiones OR dentro de una clausula
;
; Pruebas:
; (or-exp->rest (make-or-exp (make-pos-lit 2) (make-neg-lit 3)))  => (neg-lit 3)
; (or-exp->rest (make-or-exp (make-pos-lit 1) (make-or-exp (make-neg-lit 2) (make-pos-lit 3)))) => (or-exp (neg-lit 2) (pos-lit 3))
(define or-exp->rest
   (lambda (or-exp)
      (caddr or-exp)
   )
)





; ---- clausula ----

; Constructr: make-clause
;
; Crea una clausula a partir de una lista de literales lits
; Se usa para construir una clausula de la FNC
;
; Pruebas:
; (make-clause (make-pos-lit 1)) => (clause (pos-lit 1))
; (make-clause (make-or-exp (make-pos-lit 1) (make-neg-lit 2))) => (clause (or-exp (pos-lit 1) (neg-lit 2)))
(define (make-clause lits)
   (list 'clause lits)
)

; Extractor: clause->lits
;
; Extrae la lista de literales lits de una clausula
; Se usa para acceder al contenido interno de una clausula
; Es auxiliar de funciones que procesan o recorren clausulas
;
; Pruebas:
; (clause->lits (make-clause (make-pos-lit 3)))  => (pos-lit 3)
; (clause->lits (make-clause (make-or-exp (make-pos-lit 1) (make-neg-lit 2))))
;     => (or-exp (pos-lit 1) (neg-lit 2))
(define clause->lits
   (lambda (clause)
      (cadr clause)
   )
)





; ---- list-clausula ----

; Constructor: make-and-exp
;
; Crea una expresion AND entre una clausula c y el resto de clausulas rest
; Se usa para encadenar clausulas en la formula FNC
;
; Pruebas:
; (make-and-exp (make-clause (make-pos-lit 1)) (make-clause (make-neg-lit 2))) => (and-exp (clause (pos-lit 1)) (clause (neg-lit 2)))
; (make-and-exp (make-clause (make-neg-lit 3)) (make-clause (make-pos-lit 5))) => (and-exp (clause (neg-lit 3)) (clause (pos-lit 5)))
(define (make-and-exp c rest)
   (list 'and-exp c rest)
)

; Extractor: and-exp->c
;
; Extrae la primera clausula c de una and-exp
; Se usa para acceder a la clausula izquierda de una expresion AND
; Es auxiliar de funciones que recorren la lista de clausulas de la formula
;
; Pruebas:
; (and-exp->c (make-and-exp (make-clause (make-pos-lit 1)) (make-clause (make-neg-lit 2)))) => (clause (pos-lit 1))
; (and-exp->c (make-and-exp (make-clause (make-neg-lit 5)) (make-clause (make-pos-lit 3)))) => (clause (neg-lit 5))
(define and-exp->c
   (lambda (and-exp)
      (cadr and-exp)
   )
)

; Extractor: and-exp->rest
;
; Extrae el resto de clausulas de una and-exp
; Se usa para continuar el recorrido de la lista de clausulas
; Es auxiliar de funciones que recorren la lista de clausulas de la formula
;
; Pruebas:
; (and-exp->rest (make-and-exp (make-clause (make-pos-lit 1)) (make-clause (make-neg-lit 2)))) => (clause (neg-lit 2))
; (and-exp->rest (make-and-exp (make-clause (make-pos-lit 1))
;                                (make-and-exp (make-clause (make-neg-lit 2))
;                                              (make-clause (make-pos-lit 3))))) => (and-exp (clause (neg-lit 2)) (clause (pos-lit 3)))
(define and-exp->rest
   (lambda (and-exp)
      (caddr and-exp)
   )
)





; ---- fnc ----

; Constructor: make-formula
;
; Crea una formula FNC con n variables y cls como lista de clausulas.
; Se usa para construir la formula completa en FNC.
;
; Pruebas:
; (make-formula 2 (make-clause (make-pos-lit 1))) => (formula 2 (clause (pos-lit 1)))
; (make-formula 3 (make-and-exp (make-clause (make-pos-lit 1)) (make-clause (make-neg-lit 2)))) => (formula 3 (and-exp (clause (pos-lit 1)) (clause (neg-lit 2))))
(define (make-formula n cls)
   (list 'formula n cls)
)

; Extractor: formula->n
;
; Extrae el numero de variables n de una formula FNC.
; Se usa para conocer cuantas variables distintas tiene la formula.
; Es auxiliar de funciones que necesitan el numero de variables de la formula.
;
; Pruebas:
; (formula->n (make-formula 5 (make-clause (make-pos-lit 1))))  => 5
; (formula->n (make-formula 3 (make-clause (make-neg-lit 2))))  => 3
(define formula->n
   (lambda (formula)
      (cadr formula)
   )
)

; Extractor: formula->cls
;
; Extrae la lista de clausulas cls de una formula FNC.
; Se usa para acceder a todas las clausulas de la formula.
; Es auxiliar de funciones que recorren o procesan las clausulas de la formula.
;
; Pruebas:
; (formula->cls (make-formula 2 (make-clause (make-pos-lit 1)))) => (clause (pos-lit 1))
; (formula->cls (make-formula 3 (make-and-exp (make-clause (make-pos-lit 1))
;                                               (make-clause (make-neg-lit 2))))) => (and-exp (clause (pos-lit 1)) (clause (neg-lit 2)))
(define formula->cls
  (lambda (formula)
    (caddr formula)))








; --- PARSER

; parse-lit
; Parsea un numero entero y lo convierte en un lit-exp (positivo o negativo).
; Es el punto de entrada para construir literales individuales desde datos crudos.
; > (parse-lit 3)   => '(pos-lit 3)
; > (parse-lit -5)  => '(neg-lit -5)
(define parse-lit
  (lambda (dato)
    (cond
      [(and (number? dato) (positive? dato)) (make-pos-lit dato)]
      [(and (number? dato) (negative? dato)) (make-neg-lit dato)])))

; parse-listlit                             
; Parsea una lista de literales separados por 'or en una estructura list-lit.
; Se usa para procesar el contenido interno de una clausula.
; > (parse-listlit '(1))       => '(pos-lit 1)
; > (parse-listlit '(1 or -2)) => '(or-exp (pos-lit 1) (neg-lit -2))
(define (parse-listlit dato)
  (cond
    [(and (number? (car dato)) (null? (cdr dato))) (parse-lit (car dato))]
    [(eqv? (cadr dato) 'or)
     (make-or-exp (parse-lit (car dato))
                  (parse-listlit (cddr dato)))]))

; parse-clausula                               
; Parsea una lista de literales y la envuelve en una clausula.
; Se usa para construir cada clausula individual de la FNC.
; > (parse-clausula '(1))       => '(clause (pos-lit 1))
; > (parse-clausula '(1 or -2)) => '(clause (or-exp (pos-lit 1) (neg-lit -2)))
(define parse-clausula
  (lambda (dato)
    (make-clause (parse-listlit dato))))

; parse-listclausula                                            
; Parsea una secuencia de clausulas separadas por 'and en una estructura list-clausula.
; Se usa para procesar el cuerpo completo de la formula FNC.
; > (parse-listclausula '((1)))
;     => '(clause (pos-lit 1))
; > (parse-listclausula '((1) and (-2)))
;     => '(and-exp (clause (pos-lit 1)) (clause (neg-lit -2)))
(define parse-listclausula
  (lambda (dato)
    (cond
      [(and (list? (car dato)) (null? (cdr dato)))
       (parse-clausula (car dato))]
      [(eqv? (cadr dato) 'and)
       (make-and-exp (parse-clausula (car dato))
                     (parse-listclausula (cddr dato)))])))

; parse-fnc  /  PARSEBNF
; Parsea una expresion FNC completa desde su representacion como lista de datos.
; Es la funcion principal del parser; punto de entrada para convertir una FNC cruda.
; > (parse-fnc '(FNC F1 ((1))))
;     => '(formula F1 (clause (pos-lit 1)))
; > (parse-fnc '(FNC F1 ((1 or -2) and (-3))))
;     => '(formula F1 (and-exp (clause (or-exp (pos-lit 1) (neg-lit -2))) (clause (neg-lit -3))))
(define parse-fnc
  (lambda (dato)
    (make-formula (cadr dato)
                  (parse-listclausula (caddr dato)))))

(define PARSEBNF parse-fnc)

; --- UNPARSER

; unparse-lit                                                
; Convierte un lit-exp parseado de vuelta a su valor numerico original.
; Se usa como paso base al reconstruir la representacion de lista de una FNC.
; > (unparse-lit '(pos-lit 3))   => 3
; > (unparse-lit '(neg-lit -5))  => -5
(define (unparse-lit exp)
  (cond
    [(eqv? (car exp) 'pos-lit) (lit-exp->n exp)]
    [(eqv? (car exp) 'neg-lit) (lit-exp->n exp)]))

; unparse-listlit                                      
; Convierte un list-lit parseado de vuelta a una lista de numeros separados por 'or.
; Se usa para reconstruir el contenido interno de una clausula.
; > (unparse-listlit '(pos-lit 1))                          => '(1)
; > (unparse-listlit '(or-exp (pos-lit 1) (neg-lit -2)))    => '(1 or -2)
(define (unparse-listlit exp)
  (cond
    [(eqv? (car exp) 'or-exp)
     (cons (unparse-lit (or-exp->l exp))
           (cons 'or (unparse-listlit (or-exp->rest exp))))]
    [else
     (list (unparse-lit exp))]))

; unparse-clausula                                         
; Convierte una clausula parseada de vuelta a su representacion como lista anidada.
; Se usa para reconstruir cada clausula individual al deshacer la FNC.
; > (unparse-clausula '(clause (pos-lit 1)))                        => '((1))
; > (unparse-clausula '(clause (or-exp (pos-lit 1) (neg-lit -2))))  => '((1 or -2))
(define (unparse-clausula exp)
  (list (unparse-listlit (clause->lits exp))))

; unparse-list-clausula                    
; Convierte una list-clausula parseada de vuelta a su secuencia de clausulas con 'and.
; Se usa para reconstruir el cuerpo completo de la formula al deshacer la FNC.
; > (unparse-list-clausula '(clause (pos-lit 1)))
;     => '((1))
; > (unparse-list-clausula '(and-exp (clause (pos-lit 1)) (clause (neg-lit -2))))
;     => '((1) and (-2))
(define (unparse-list-clausula exp)
  (cond
    [(eqv? (car exp) 'and-exp)
     (cons (unparse-clausula (and-exp->c exp))
           (cons 'and (unparse-list-clausula (and-exp->rest exp))))]
    [else
     (unparse-clausula exp)]))

; unparse-fnc  /  UNPARSEBNF
; Convierte una formula FNC parseada de vuelta a su representacion original como lista.
; Es la funcion principal del unparser; inversa exacta de parse-fnc.
; > (unparse-fnc (parse-fnc '(FNC F1 ((1)))))
;     => '(FNC F1 (1))
; > (unparse-fnc (parse-fnc '(FNC F1 ((1 or -2) and (-3)))))
;     => '(FNC F1 (1 or -2) and (-3))
(define (unparse-fnc exp)
  (cons 'FNC
        (cons (formula->n exp)
              (unparse-list-clausula (formula->cls exp)))))

(define UNPARSEBNF unparse-fnc)