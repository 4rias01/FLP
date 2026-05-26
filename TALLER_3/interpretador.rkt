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
    (expresion ("declarar" "("
                 (arbno identificador "=" expresion ";") ")" "{" expresion "}")
                variableLocal-exp)
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")"
                                "{" expresion "}") procedimiento-exp)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")"
                          "finEval") app-exp)

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
                 (eval-expresion cuerpo (ambiente-inicial))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))
(define ambiente-inicial
  (lambda ()
    (extender-amb
     '(@a @b @c @d @e)
     (list 1 2 3 "HOLA" "FLP")
     (ambiente-vacio))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expresion
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable amb id))
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (let ((args (eval-rands (list exp1 exp2) amb)))
                         (aplicar-primitiva-binaria
                          prim-binaria
                          args)))
      (primapp-un-exp (primitiva-unaria expresion) (aplicar-primitiva-unaria
                                                    primitiva-unaria
                                                    (eval-expresion expresion amb)))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (true-value? (eval-expresion test-exp amb))
                           (eval-expresion true-exp amb)
                           (eval-expresion false-exp amb)))
      (variableLocal-exp (ids exps cuerpo)
                         (let ((args (eval-rands exps amb)))
                           (eval-expresion cuerpo
                                           (extender-amb ids args amb))))
      (procedimiento-exp (ids cuerpo) (cerradura
                                       ids
                                       cuerpo
                                       amb))
      (app-exp (exp exps) (let ((proc (eval-expresion exp amb))
                                (args (eval-rands exps amb)))
                            (if (procVal? proc)
                                (aplicar-procedimiento proc args)
                                (eopl:error 'eval-expresion
                                 "Attempt to apply non-procedure ~s" proc))
                            ))
      )))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands amb)
    (map (lambda (x) (eval-rand x amb)) rands)))

(define eval-rand
  (lambda (rand amb)
    (eval-expresion rand amb)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define aplicar-primitiva-binaria
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

(define aplicar-primitiva-unaria
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
(define-datatype procVal procVal?
  (cerradura
   (ids (list-of symbol?))
   (cuerpo expresion?)
   (amb ambiente?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define aplicar-procedimiento
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids cuerpo amb)
               (eval-expresion cuerpo (extender-amb ids args amb))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype ambiente ambiente?
  (vacio)
  (extendido (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (amb ambiente?))
  (extendido-recursivo (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (cuerpos (list-of expresion?))
                                   (amb ambiente?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define ambiente-vacio  
  (lambda ()
    (vacio)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extender-amb
  (lambda (syms vals amb)
    (extendido syms vals amb)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extender-amb-rec
  (lambda (proc-names idss cuerpos amb-viejo)
    (extendido
     proc-names idss cuerpos amb-viejo)))


;función que busca un símbolo en un ambiente
(define buscar-variable
  (lambda (amb sym)
    (cases ambiente amb
      (vacio ()
             (eopl:error 'ambiente-vacio "No binding for ~s" sym))
      (extendido (syms vals amb-viejo)
                 (let ((pos (list-find-position sym syms)))
                   (if (number? pos)
                       (list-ref vals pos)
                       (buscar-variable amb-viejo sym))))
      (extendido-recursivo (nombres-proc idss cuerpos amb-viejo)
                           (let ((pos (list-find-position sym nombres-proc)))
                             (if (number? pos)
                                 (cerradura (list-ref idss pos)
                                            (list-ref cuerpos pos)
                                            amb)
                                 (buscar-variable amb-viejo sym)))))))


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