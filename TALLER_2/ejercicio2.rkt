#lang eopl


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
; no hacemos uno para single-lit porque es el mismo que el de lit-exp, ya que un single-lit es un lit-exp :3

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
; no hacemos el de single-cl porque es el mismo que el de clausula, ya que un single-cl es una clausula :3

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

;;---------------------------------------------------------------

;; Parser de literal <lit-exp>
(define (make-pos-lit n)
     (list 'pos-lit n)
)

(define (make-neg-lit n)
     (list 'neg-lit n)
)

(define parse-lit
  (lambda (dato)
    (cond
      [ (and (positive? dato)(number? dato)) (make-pos-lit dato)]
      [ (and (negative? dato)(number? dato)) (make-neg-lit dato)] )))


;; Parser de lista de literales <list-lit>
(define parse-listlit ???)


;; Parder de Or Expresion <or-exp>
(define (make-or-exp l rest)
     (list 'or-exp l rest)
)

(define parse-or
  (lambda (dato)
    (make-or-exp
     (parse-lit (car dato))
     (parse-listlit (caddr dato)) )  ))


(define parse-clausula ???)
(define parse-and ???)
(define parse-listclausula ???)

(define PARSEBNF ???)
