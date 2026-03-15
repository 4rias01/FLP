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



;;---------------------------------------------------------------

;; Parser de literal <lit-exp> 
(define parse-lit
  (lambda (dato)
    (cond
      [ (and (positive? dato)(number? dato)) (make-pos-lit dato)]
      [ (and (negative? dato)(number? dato)) (make-neg-lit dato)] )))


;; Parser de lista de literales <list-lit>
(define parse-listlit
  (lambda (dato)
    (cond
      [ (number? dato) (parse-lit dato)]
      [ (null? (cdr dato)) (parse-lit (car dato)) ] ;;Tercer caso para manejar listas de un solo elemento
      [ (eqv? (cadr dato) 'or) (parse-or dato) ] )  )) 


;; Parder de Or Expresion <or-exp>
(define parse-or
  (lambda (dato)
    (make-or-exp
     (parse-lit (car dato))
     (parse-listlit (cddr dato)) )  ))
