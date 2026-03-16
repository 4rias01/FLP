#lang eopl


;; terminal -> se lee directo
;; NO terminal -> Requiere su propio PARSER.
;;
;; Gramatica:
;;
;     <lit-exp>   ::=  <int>
;                 ::= -<int>

;     <or-exp>    ::= (<lit-exp> or <list-lit>)

;     <list-lit>  ::= <lit-exp>
;                 ::= <or-exp>

;     <clausula>  ::= <list-lit>

;     <and-exp>   ::= <clausula> and <list-clausula>

; <list-clausula> ::= <clausula>
;                 ::= <and-exp>

;       <fnc-exp> ::= (<int> <list-clausula>)

; 1. Implementacion por medio de DATATYPES (Hecho en base a los datatypes del segundo punto)

(define-datatype dt-lit-exp dt-lit-exp?
  (make-pos-lit 
      (n number?)
   )

  (make-neg-lit 
      (n number?)
   )
)

(define-datatype dt-list-lit dt-list-lit?
  (make-single-lit 
      (l dt-lit-exp?)
   )

  (make-or-exp  
      (l dt-lit-exp?) 
      (rest dt-list-lit?)
   )
) 

(define-datatype dt-clausula dt-clausula?
  (make-clause
      (lits dt-list-lit?)
   )
)

(define-datatype dt-list-clausula dt-list-clausula?
  (make-single-cl
      (c dt-clausula?)
   )

  (make-and-exp
      (c dt-clausula?) 
      (rest dt-list-clausula?)
   )
)

(define-datatype dt-fnc dt-fnc?
  (make-formula
      (n number?) 
      (cls dt-list-clausula?)
   )
)

;;---------------------------------------------------------------

;; Parser de literal <lit-exp> 
(define parse-lit
  (lambda (dato)
    (cond
      [ (and (positive? dato)(number? dato)) (make-pos-lit dato)]
      [ (and (negative? dato)(number? dato)) (make-neg-lit dato)] )))


;; Parser de lista de literales <list-lit>
(define (parse-listlit dato)
   (cond
      [(number? dato) (make-single-lit (parse-lit dato))]
      [(eqv? (cadr dato) 'or) (make-or-exp (parse-lit (car dato)) (parse-listlit (caddr dato)))]
      )
)

;; Parser de Clausula
(define parse-clausula
  (lambda (dato)
    (make-clause (parse-listlit dato)) ))


;; Parser de list-clausula
(define parse-listclausula
  (lambda (dato)
    (cond
      [(and (list? (car dato)) (null? (cdr dato))) (make-single-cl (parse-clausula (car dato)))]
      [else (make-and-exp  (parse-clausula (car dato)) (parse-listclausula (cddr dato)))]
      )
    ))

;; Parser de FNC
(define parse-fnc
  (lambda (dato)
    (make-formula (cadr dato) (parse-listclausula (caddr dato)))))


;; UNPARSER

