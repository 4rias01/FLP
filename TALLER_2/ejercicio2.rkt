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


(define PARSEBNF ???)

(define parse-lit
  (lamda (dato)
         ???))

(define parse-or ???)
(define parse-listlit ???)
(define parse-clausula ???)
(define parse-and ???)
(define parse-listclausula ???)
