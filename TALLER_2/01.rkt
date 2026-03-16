#lang eopl

; 0. Gramatica

;     <lit-exp>   ::=  <int>
;                 ::= -<int>

;     <or-exp>    ::= <lit-exp> or <list-lit>

;     <list-lit>  ::= <lit-exp>
;                 ::= (or-exp)

;     <clausula>  ::= (<list-lit>)

;     <and-exp>   ::= <clausula> and <list-clausula>

; <list-clausula> ::= <clausula>
;                 ::= <and-exp>

;       <fnc-exp> ::= (<int> <list-clausula>)

; 1. Implementacion por medio de Constructores y Extractores (Hecho en base a los datatypes del segundo punto)

; lit-exp

; --- constructores
(define (make-pos-lit n)
     (list 'pos-lit n)
)

(define (make-neg-lit n)
     (list 'neg-lit n)
)

; --- predicados
(define (post-lit? exp)
     (and
          (pair? exp) 
          (eq? (car exp) 'pos-lit) 
          (number? (cadr exp))
      )
)

(define (neg-lit? exp)
     (and
          (pair? exp) 
          (eq? (car exp) 'neg-lit) 
          (number? (cadr exp))
      )
)

;; --- predicado general de lit-exp (útil cuando no se usan datatypes)
(define (lit-exp? exp)
  (or (post-lit? exp) (neg-lit? exp)))

; --- extractor
(define lit-exp->n
     (lambda (lit-exp)
          (cadr lit-exp)
     )
)

; list-lit

; --- constructores
; no hacemos uno para single-lit porque es el mismo que el de lit-exp, ya que un single-lit es un lit-exp

(define (make-or-exp l rest)
     (list 'or-exp l rest)
)

; --- predicados
(define (single-lit? exp)
   (lit-exp? exp)
)

(define (or-exp? exp)
     (and
         (pair? exp)
         (eq? (car exp) 'or-exp)
         (lit-exp? (cadr exp))
         (list-lit? (caddr exp))
      )
)

(define (list-lit? exp)
   (or
      (single-lit? exp)
      (or-exp? exp)
   )
)

; --- extractores
(define list-lit->l
     (lambda (list-lit)
          (cadr list-lit)
     )
)

; or-exp

; --- extractores, solo estos porque los demas ya estan en list-lit
(define or-exp->l
   (lambda (or-exp)
      (cadr or-exp)
   )
)

(define or-exp->rest
   (lambda (or-exp)
      (caddr or-exp)
   )
)

; clausula

; --- constructores
(define (make-clause lits)
   (list 'clause lits)
)

; --- predicados
(define (clause? exp)
   (and
      (pair? exp)
      (eq? (car exp) 'clause)
      (list-lit? (cadr exp))
   )
)

; --- extractores
(define clause->lits
   (lambda (clause)
      (cadr clause)
   )
)

; list-clausula

; --- constructores
; no hacemos el de single-cl porque es el mismo que el de clausula, ya que un single-cl es una clausula

(define (make-and-exp c rest)
   (list 'and-exp c rest)
)

; --- predicados
(define (single-cl? exp)
   (clause? exp)
)

(define (and-exp? exp)
   (and
      (pair? exp)
      (eq? (car exp) 'and-exp)
      (clause? (cadr exp))
      (list-clausula? (caddr exp))
   )
)

(define (list-clausula? exp)
  (or
     (clause? exp)
     (and-exp? exp)))

; --- extractores
(define and-exp->c
   (lambda (and-exp)
      (cadr and-exp)
   )
)

(define and-exp->rest
   (lambda (and-exp)
      (caddr and-exp)
   )
)

; fnc

; --- constructores
(define (make-formula n cls)
   (list 'formula n cls)
)

; --- predicados
(define (formula? exp)
   (and
      (pair? exp)
      (eq? (car exp) 'formula)
      (number? (cadr exp))
      (list-clausula? (caddr exp))
   )
)

; --- extractores
(define formula->n
   (lambda (formula)
      (cadr formula)
   )
)

(define formula->clausulas
  (lambda (formula)
    (caddr formula)))

; 2. Implementacion por medio de Datatypes (Una forma diferente de representar la gramatica sin usar constructores y extractores a mano)

(define-datatype dt-lit-exp dt-lit-exp?
  (dt-pos-lit 
      (n number?)
   )

  (dt-neg-lit 
      (n number?)
   )
)

(define-datatype dt-list-lit dt-list-lit?
  (dt-single-lit 
      (l dt-lit-exp?)
   )

  (dt-or-exp     
      (l dt-lit-exp?) 
      (rest dt-list-lit?)
   )
) 

(define-datatype dt-clausula dt-clausula?
  (dt-clause
      (lits dt-list-lit?)
   )
)

(define-datatype dt-list-clausula dt-list-clausula?
  (dt-single-cl
      (c dt-clausula?)
   )

  (dt-and-exp
      (c dt-clausula?) 
      (rest dt-list-clausula?)
   )
)

(define-datatype dt-fnc dt-fnc?
  (dt-formula
      (n number?) 
      (cls dt-list-clausula?)
   )
)

;; -----------------------------------------------------------------;
;; Ejemplos de construcción (manual vs datatypes)
;; -----------------------------------------------------------------;

;; 1) Construcción manual (constructores + predicados)
(define ex-pos-lit (make-pos-lit 5))
(define ex-neg-lit (make-neg-lit 7))
(define ex-or-lit (make-or-exp ex-pos-lit (make-or-exp ex-neg-lit ex-pos-lit)))
(define ex-clause (make-clause ex-or-lit))
(define ex-and (make-and-exp ex-clause (make-and-exp ex-clause ex-clause)))
(define ex-formula (make-formula 10 ex-and))

;; 2) Construcción con datatypes
(define dt-pos-lit-val (dt-pos-lit 11))
(define dt-or-lit-val (dt-or-exp dt-pos-lit-val (dt-single-lit dt-pos-lit-val)))
(define dt-clause-val (dt-clause dt-or-lit-val))
(define dt-and-val (dt-and-exp dt-clause-val (dt-single-cl dt-clause-val)))
(define dt-formula-val (dt-formula 22 dt-and-val))

;; Impresiones simples para verificar el formato
(display "-- Ejemplos construidos (verificar por inspección)--")
(newline)
(display ex-formula)
(newline)
(display dt-formula-val)
(newline)

