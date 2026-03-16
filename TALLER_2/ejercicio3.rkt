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

; Datatypes

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

; --- PARSER POR DATATYPES

;; Parser de literal <lit-exp> 
(define parse-lit
  (lambda (dato)
    (cond
      [ (and (positive? dato) (number? dato)) (dt-pos-lit dato)]
      [ (and (negative? dato) (number? dato)) (dt-neg-lit (abs dato))] )))


;; Parser de lista de literales <list-lit>
(define (parse-listlit dato)
   (cond
      [(number? dato) (dt-single-lit (parse-lit dato))]
      [(and (list? dato) (null? (cdr dato))) (dt-single-lit (parse-lit (car dato)))]
      [(eqv? (cadr dato) 'or)
       (dt-or-exp (parse-lit (car dato)) (parse-listlit (cddr dato)))]
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

; --- UNPARSER POR DATATYPES

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

(define i0 (PARSEBNF '(FNC 5 ( (1 or -2 or 3) and (4 or 5) ) )))
(displayln i0)


(define i1 (UNPARSEBNF (PARSEBNF '(FNC 5 ( (1 or -2 or 3) and (4 or 5) ) )) ))
(displayln i1)

; --- RESOLUTOR

;; Auxiliar para poder hacer esa vaina. Añade el valor a todas las listas de la lista de listas 
(define (agregar-a-todos val listas)
   (if (null? listas)
       '()
       (cons (cons val (car listas))
             (agregar-a-todos val (cdr listas)))))

;; genera todas las combinaciones posibles de n booleanos de forma recursiva gracias a la funcion de arriba
(define (all-assignments n)
   (if (zero? n)
       (list '())

       (let ([rest (all-assignments (sub1 n))])
          (append
             (agregar-a-todos #f rest)
             (agregar-a-todos #t rest)))))

;; Solo porque santi me lo pide
(define (csq-list-ref lst i)
   (if (= i 0)
       (car lst)
       (csq-list-ref (cdr lst) (- i 1))
     )
)

;; Evalúa un literal usando una asignación (lista de booleanos indexada desde 1).
(define (eval-lit lit asign)
  (cases dt-lit-exp lit
    (dt-pos-lit (n) (csq-list-ref asign (sub1 n))) ;; obtenemos el valor de la variable nesima (indexada desde 1)
    (dt-neg-lit (n) (not (csq-list-ref asign (sub1 n)))))) ;; la negacion del valor

;; evaluacion de <list-lit> (or's practicamente)
(define (eval-listlit lits asign)
  (cases dt-list-lit lits
    (dt-single-lit (l) (eval-lit l asign))
    (dt-or-exp (l rest)
      (or (eval-lit l asign)
          (eval-listlit rest asign)))))

;; Evalúa una cláusula (un paréntesis con una lista de literales)
(define (eval-clausula cl asign)
  (cases dt-clausula cl
    (dt-clause (lits) (eval-listlit lits asign))))

;; Evalúa una lista de cláusulas (AND entre cláusulas)
(define (eval-list-clausula cls asign)
  (cases dt-list-clausula cls
    (dt-single-cl (c) (eval-clausula c asign))
    (dt-and-exp (c rest)
      (and (eval-clausula c asign)
           (eval-list-clausula rest asign)))))

;; auxiliar que realiza las evaluaciones de las asignaciones sobre la formula, devuelve true si alguna asignacion satisface la formula
(define (buscar asigs cls)
   (cond
      [(null? asigs) (list 'insatisfactible '())]
      [(eval-list-clausula cls (car asigs))
       (list 'satisfactible (car asigs))]
      [else (buscar (cdr asigs) cls)]
     )
)

;; Basicamente usamos el buscar con todas las asignaciones posibles para ver si alguna satisface la formula, si es asi devuelve satisfactible y la asignacion, sino devuelve insatisfactible
(define (EVALUARSAT fnc)
   (let ([f (if (and (list? fnc) (eqv? (car fnc) 'FNC))
               (PARSEBNF fnc)
               ((error "EVALUARSAT: no es FNC" fnc)))])
      (cases dt-fnc f
         (dt-formula (n cls)
            (buscar (all-assignments n) cls)))
     )
)

;; ejemplos cortos para verificar resultado >:)
(displayln (EVALUARSAT '(FNC 4 ((1 or -2 or 3 or 4) and (-2 or 3) and (-1 or -2 or -3) and (3 or 4) and (2)))))
(displayln (EVALUARSAT '(FNC 2 ((1 or 2) and (-1) and (-2)))))
(displayln (EVALUARSAT '(FNC 1 ((1)) )))