#lang eopl
(require racket/string)
;******************************************************************************************



;Especificación Léxica

(define scanner-spec-simple-interpreter
 '((white-sp
    (whitespace) skip)
   (comentario
    ("%" (arbno (not #\newline))) skip)
   (identificador
    ("@" letter (arbno (or letter digit "?"))) symbol)
   (numero
    (digit (arbno digit)) number)
   (numero
    (digit (arbno digit) "." (arbno digit)) number)
   (numero
    ("-" digit (arbno digit)) number)
   (numero
    ("-" digit (arbno digit) "." (arbno digit)) number)
   (texto
    (letter (arbno (or letter digit "_"))) string)
   ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((programa (expresion) un-programa)
    (expresion (numero) numero-lit)
    (expresion ("\"" texto "\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)
    (expresion ("Si" expresion "{" expresion "}" "sino" "{" expresion "}")
               condicional-exp)
    (expression ("declarar"
                 (arbno identificador "=" expresion ";") expresion)
                variableLocal-exp)

    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria ("!=") primitiva-diferente)
    (primitiva-binaria ("==") primitiva-comparador-igual)

    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-negacion-booleana)
    (primitiva-unaria ("longitud") primitiva-longitud)
    ))


;*******************************************************************************************
; DATATYPES hecho automaticamente

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (cuerpo)
                 (eval-expresion cuerpo (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     (list 1 2 3 "HOLA" "FLP")
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (apply-env env id))
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (let ((args (eval-rands (list exp1 exp2) env)))
                         (apply-primitiva-binaria
                          prim-binaria
                          args)))
      (primapp-un-exp (primitiva-unaria expresion) (apply-primitiva-unaria
                                                    primitiva-unaria
                                                    (eval-expresion expresion env)))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (true-value? (eval-expresion test-exp env))
                           (eval-expresion true-exp env)
                           (eval-expresion false-exp env)))
      (variableLocal-exp (ids exps cuerpo)
                         (let ((args (eval-rands exps env)))
                           (eval-expresion cuerpo
                                           (extend-env args ids env))))
      )))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expresion rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitiva-binaria
  (lambda (prim args)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ (car args) (cadr args)))
      (primitiva-resta () (- (car args) (cadr args)))
      (primitiva-div () (/ (car args) (cadr args)))
      (primitiva-multi () (* (car args) (cadr args)))
      (primitiva-concat () (string-append (car args) (cadr args)))
      (primitiva-mayor () (if (> (car args) (cadr args)) 1 0))
      (primitiva-menor () (if (< (car args) (cadr args)) 1 0))
      (primitiva-mayor-igual () (if (>= (car args) (cadr args)) 1 0))
      (primitiva-menor-igual () (if (<= (car args) (cadr args)) 1 0))
      (primitiva-diferente () (if (equal? (car args) (cadr args)) 0 1))
      (primitiva-comparador-igual () (if (equal? (car args) (cadr args)) 1 0))
      )))

(define apply-primitiva-unaria
  (lambda (prim arg)
    (cases primitiva-unaria prim
      (primitiva-add1 () (+ arg 1))
      (primitiva-sub1 () (- arg 1))
      (primitiva-negacion-booleana () (if (equal? arg 0) 1 0))
      (primitiva-longitud () (string-length arg))
      )
    ))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expresion body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))


;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;******************************************************************************************
;Pruebas

(show-the-datatypes)
just-scan
scan&parse



(interpretador)