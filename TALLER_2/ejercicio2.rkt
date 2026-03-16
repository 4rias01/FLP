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

; 1. Implementacion por medio de Constructores y Extractores (Hecho en base a los datatypes del segundo punto)

; lit-exp
; --- constructores
(define (make-pos-lit n)
     (list 'pos-lit n)
)

(define (make-neg-lit n)
     (list 'neg-lit n)
)

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

; --- extractores
(define formula->n
   (lambda (formula)
      (cadr formula)
   )
)

(define formula->cls
  (lambda (formula)
    (caddr formula)))

; --- PARSER

;; Parser de literal <lit-exp>
(define parse-lit
  (lambda (dato)
    (cond
      [(and (number? dato) (positive? dato)) (make-pos-lit dato)]
      [(and (number? dato) (negative? dato)) (make-neg-lit dato)])))

;; Parser de lista de literales <list-lit>
(define (parse-listlit dato)
  (cond
    [(and (number? (car dato)) (null? (cdr dato))) (parse-lit (car dato))]           ; single-lit = el lit-exp mismo
    [(eqv? (cadr dato) 'or)
     (make-or-exp (parse-lit (car dato))
                  (parse-listlit (cddr dato)))]))

;; Parser de clausula
(define parse-clausula
  (lambda (dato)
    (make-clause (parse-listlit dato))))

;; Parser de list-clausula
(define parse-listclausula
  (lambda (dato)
    (cond
      [(and (list? (car dato)) (null? (cdr dato)))
       (parse-clausula (car dato))]             ; single-cl = la clausula misma
      [(eqv? (cadr dato) 'and)
       (make-and-exp (parse-clausula (car dato))
                     (parse-listclausula (cddr dato)))])))

;; Parser de FNC
(define parse-fnc
  (lambda (dato)
    (make-formula (cadr dato)
                  (parse-listclausula (caddr dato)))))

(define PARSEBNF parse-fnc)

; --- UNPARSER

;; Unparser de lit-exp
(define (unparse-lit exp)
  (cond
    [(eqv? (car exp) 'pos-lit) (lit-exp->n exp)]
    [(eqv? (car exp) 'neg-lit) (lit-exp->n exp)]))

;; Unparser de list-lit
(define (unparse-listlit exp)
  (cond
    [(eqv? (car exp) 'or-exp)
     (cons (unparse-lit (or-exp->l exp))
           (cons 'or (unparse-listlit (or-exp->rest exp))))]
    [else                                       ; single-lit: es un lit-exp directo
     (list (unparse-lit exp))]))

;; Unparser de clausula
(define (unparse-clausula exp)
  (list (unparse-listlit (clause->lits exp))))

;; Unparser de list-clausula
(define (unparse-list-clausula exp)
  (cond
    [(eqv? (car exp) 'and-exp)
     (cons (unparse-clausula (and-exp->c exp))
           (cons 'and (unparse-list-clausula (and-exp->rest exp))))]
    [else                                       ; single-cl: es una clausula directa
     (unparse-clausula exp)]))

;; Unparser de FNC
(define (unparse-fnc exp)
  (cons 'FNC
        (cons (formula->n exp)
              (unparse-list-clausula (formula->cls exp)))))

(define UNPARSEBNF unparse-fnc)