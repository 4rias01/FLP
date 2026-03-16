#lang eopl
(require racket/base racket/list)

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

; --- PARSER

;; Parser de literal <lit-exp> 
(define parse-lit
  (lambda (dato)
    (cond
      [ (and (positive? dato)(number? dato)) (dt-pos-lit dato)]
      [ (and (negative? dato)(number? dato)) (dt-neg-lit dato)] )))


;; Parser de lista de literales <list-lit>
(define (parse-listlit dato)
   (cond
      [(number? dato) (dt-single-lit (parse-lit dato))]
      [(eqv? (cadr dato) 'or) (dt-or-exp (parse-lit (car dato)) (parse-listlit (caddr dato)))]
      )
)

;; Parser de Clausula
(define parse-clausula
  (lambda (dato)
    (dt-clause (parse-listlit dato)) ))


;; Parser de list-clausula
(define parse-listclausula
  (lambda (dato)
    (cond
      [(and (list? (car dato)) (null? (cdr dato))) (dt-single-cl (parse-clausula (car dato)))]
      [else (dt-and-exp  (parse-clausula (car dato)) (parse-listclausula (cddr dato)))]
      )
    ))

;; Parser de FNC
(define parse-fnc
  (lambda (dato)
    (dt-formula (cadr dato) (parse-listclausula (caddr dato)))))

(define PARSEBNF parse-fnc)

; --- UNPARSER

;; Unparser de lit-exp
(define (unparse-lit exp)
   (cases dt-lit-exp exp
      (dt-pos-lit (n) n)
      (dt-neg-lit (n) n)
   )
)

(define (unparse-listlit exp)
   (cases dt-list-lit exp
      (dt-single-lit (l) 
         (list (unparse-lit l)))    
      (dt-or-exp (l rest) 
         (cons (unparse-lit l) 
               (cons 'or (unparse-listlit rest))))))

(define (unparse-clausula exp)
   (cases dt-clausula exp
      (dt-clause (lits) (list (unparse-listlit lits)))
     )
)

(define (unparse-list-clausula exp)
     (cases dt-list-clausula exp
          (dt-single-cl (c) (unparse-clausula c))
          (dt-and-exp (c rest) (cons (unparse-clausula c) (cons 'and (unparse-list-clausula rest))))
     )
)

(define (unparse-fnc exp)
   (cases dt-fnc exp
      (dt-formula (n cls) (cons 'FNC (cons n (unparse-list-clausula cls))))
   )
)

(define UNPARSEBNF unparse-fnc)

(define i0 (PARSEBNF '(FNC 5 ( (1 or 2 or 3) and (4 or 5) ) )))
(displayln i0)


(define i1 (UNPARSEBNF (PARSEBNF '(FNC 5 ( (1 or 2 or 3) and (4 or 5) ) )) ))
(displayln i1)