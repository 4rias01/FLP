#lang eopl
(require racket/string)

;******************************************************************************************
; Fundamentos de Lenguajes de Programación - Taller 3
; GRUPO:
;; Santiago Arias Rojas - 202416285
;; Sebastian Calvo Carvajal - 202419118
;; Juan Jose Rodriguez Lozano - 202419084
;; Link de Repositorio Github: https://github.com/4rias01/FLP
;******************************************************************************************


;; Especificación Léxica - scanner-spec-simple-interpreter
;; Esta sirve para definir las reglas que nos diran como se divide el
;; texto del programa en tokens (unidades minimas con un significado).
;; La usamos en conjunto con la especificacion gramatica para generar los distintos
;; elementos del interpretador mediante SLLGEN.

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
    ((or letter "_") (arbno (or letter digit "_" ":"))) string)
   ))



;; Especificación Sintáctica (gramática) - grammar-simple-interpreter
;; Usamos esta especificacion para definir la gramatica del lenguaje, es decir,
;; el como se deben de organizaf los tokens para conformar construcciones validas. 
;; En el interpretados se usa en conjunto con la especificacion lexica para  
;; usar SLLGEN.
;; esta conecta las reglas de la gramatica con las variantes datatypes, esto es  el
;; plano usado para generar el arbol de sintaxis abstracta (AST).

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
    (expresion ("declarar*" "[" (arbno identificador "(" (separated-list identificador ",") ")"
                                       "=" expresion) "]" "{" expresion "}") variableRecursiva-exp)

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
    (lambda (pgm) (eval-programa  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete
;*******************************************************************************************

;eval-program: <programa> -> number
;; función que evalúa un programa teniendo en cuenta un ambiente inicial
;; dado (se inicializa dentro del programa).
;; Se usa como un punto de entrada principal del interpretador
;; para evaluar el programa completo.

(define eval-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (cuerpo)
                 (eval-expresion cuerpo (ambiente-inicial))))))



;  Ambiente inicial
;; Procedimiento que crea el ambiente inicial de nuestro interpretador
;; se usa para ligar identificadores con sus valores iniciales para poder
;; usarlos desde el inicio del programa.
;; su funcion principal es ser el punto de partida para la cadena de ambientes del programa.

(define ambiente-inicial
  (lambda ()
    (extender-amb
     '(@a @b @c @d @e)
     (list 1 2 3 "HOLA" "FLP")
     (ambiente-vacio))))



;  eval-expresion: <expression> <environment> -> value
;; evalua una expresión en el ambiente dado.
;; Este procedimiento recibe una expresion en sintaxis abstracta y un ambiente
;; la expresion es analizada internamente con cases para ver que tipo de
;; produccion se esta viendo, y asi mismo evaluar la produccion.
;; el ambiente sirve como contexto de las variables y valores conocidos
;; hasta el momento de hacerse esta evaluacion.
;; Es usada inicialmente en el eval-programa, para posteriormente llamarse
;; recursivamente para evaluar todas las expresiones dadas, para finalmente
;; retornar el valor resultante de la evaluacion

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
      (procedimiento-exp (ids cuerpo) (cerradura ids cuerpo amb))
      (app-exp (exp exps) (let ((proc (eval-expresion exp amb))
                                (args (eval-rands exps amb)))
                            (if (procVal? proc)
                                (aplicar-procedimiento proc args)
                                (eopl:error 'eval-expresion
                                 "Attempt to apply non-procedure ~s" proc))
                            ))
      (variableRecursiva-exp (proc-nombres idss cuerpos declaracion-cuerpo)
                             (eval-expresion declaracion-cuerpo
                                             (extender-amb-rec proc-nombres idss cuerpos amb)))
      )))



; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
;; Este procedimiento recibe una lista de expresiones y un ambiente, y recorre
;; la lista usando un map, aplicando el procedimiento eval-rand a cada una.
;; el ambiente es usado como contexto de las variables y valores conocidos
;; al momento de hacer la evaluacion.
;; Es usada para evaluar los operandos de una expresion antes de aplicar
;; el procedimiento correspondientr.

(define eval-rands
  (lambda (rands amb)
    (map (lambda (x) (eval-rand x amb)) rands)))



;; eval-rand evalua un operando en el ambiente dado, recibe una expresion y ambiente
;; esta sencillamente llama a eval-expresion entregando la expresion (operando) y ambiente,
;; para que esa funcion la evalue dentro del ambiente actual.
;; el ambiente es usado como contexto de las variables y valores conocidos
;; al momento de hacer la evaluacion.

(define eval-rand
  (lambda (rand amb)
    (eval-expresion rand amb)))



;; aplicar-primitiva-binaria: <primitiva> <list-of-values> -> value
;; Este procedimiento recibe una primitiva binaria y una listas de
;; argumentos ya evaluados.
;; se analiza la primitiva internamente para determinar cual operacion
;; vamos a realizar, dependiendo de la primitiva realizaremos ya sea operaciones
;; aritmeticas, de comparacion o incluso de manipulacion de cadenas.
;; Esta se usa cuando evaluamos expresiones que involucren primitivas binarias
;; en nuestro eval-expresion.

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



;; aplicar-primitiva-unaria: <primitiva> <value> -> valor
;; Este procedimiento recibe una primitiva unaria y un argumento evaluado.
;; Se analiza el tipo de primitiva usando cases para saber que tipo de
;; operacion se realizara.
;; Se usa para evaluar expresiones que involucren a primitivas unarias.

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
;; Sirve la funcion de un predicado, usado para corroborar el valor booleano
;; de una expresion evaluada, en el lenguaje manejado, 0 es falso y cualquier otra expresion
;; sera verdadera.

(define true-value?
  (lambda (x)
    (not (zero? x))))



;*******************************************************************************************
;Procedimientos
;*******************************************************************************************

; procVal
;; datatype que representa los valores de tipo procedimiento dentro de nuestro
;; interpretador. Se representa con una cerradura que guarda los identificadores, cuerpo
;; y el ambiente donde fue creado.
;; Este tipo de dato es usado para representar y almacenar los elementos asociados
;; a un procedimiento dentro de nuestro interpretador

(define-datatype procVal procVal?
  (cerradura
   (ids (list-of symbol?))
   (cuerpo expresion?)
   (amb ambiente?)))



;aplicar-procedimiento: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
;; Este procedimiento recibira un procedimiento y una lista de argumentos evaluados
;; usando cases, extraemos la informacion almacenada en el procedimiento.
;; Posteriormente usando esta informacion extendemos el ambiente, añadiendo los nuevos
;; argumentos dados a los identificadores y ambiente previamente definidos en la declaracion del
;; procedimiento, finalmente evaluamos el cuerpo en este nuevo ambiente extendido.

(define aplicar-procedimiento
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids cuerpo amb)
               (eval-expresion cuerpo (extender-amb ids args amb))))))



;*******************************************************************************************
;Ambientes
;*******************************************************************************************

;definición del tipo de dato ambiente
;; datatype que representa los ambientes de nuestro interpretador
;; Un ambiente guarda la relacion entre identificadores y sus valores
;; permitiendo conocer el valor de una variable en la evaluacion.
;; puede ser un abiente vacio, se puede extender un ambiente, o tambien
;; se tiene la variable del extendido-recursivo, que se usara para manejar
;; los procedimientos recursivos.

(define-datatype ambiente ambiente?
  (vacio)
  (extendido (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (amb ambiente?)
                       )
  (extendido-recursivo (proc-nombres (list-of symbol?))
                       (idss (list-of (list-of symbol?)))
                       (cuerpos (list-of expresion?))
                       (amb ambiente?)
                       )
  )



;;scheme-value?
;; Este es un predicado para corroborar que los elementos almacenados en un ambiente
;; sean valores validos de scheme, actualmente este retorna siempre verdadero para
;; cualquier valor de scheme

(define scheme-value? (lambda (v) #t))



;ambiente-vacio:      -> enviroment
;función que crea un ambiente vacío
;; Es sencillamente usado para crear un ambiente vacio, que es el punto inicial
;; de la cadena de ambientes, este es usado por init-env el cual lo extiende.
;; Es necesario para terminar la busqueda a la hora de aplicar el apply-env

(define ambiente-vacio  
  (lambda ()
    (vacio)))       ;llamado al constructor de ambiente vacío 



;extender-amb: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
;; Este procedimiento recibe simbolos, valores y un ambiente,
;; su funcion es la de crear un nuevo ambiente extendido asociando cada
;; identificador a su respectivo valor.
;; El anterior ambiente se pasa como argumento, asegurando que se conserve
;; la cadena de ambientes para su posterior evaluacion.

(define extender-amb
  (lambda (syms vals amb)
    (extendido syms vals amb)))



;; extender-amb-rec
;; Este recibe los nombres de procedimientos, listas de parametros,
;; los cuerpos y un ambiente.
;; Su funcion es crear un ambiente extendido recursivo, con el cual los
;; procedimientos podran referenciarse a si mismos al momento de hacer la evaluacion.
;; Se usa para la declaracion de procedimientos recursivos.

(define extender-amb-rec
  (lambda (proc-nombres idss cuerpos amb-viejo)
    (extendido-recursivo proc-nombres idss cuerpos amb-viejo)))



; buscar-variable: función que busca un símbolo en un ambiente
;; Este procedimiento recibe un ambiente y un identificador, su funcion es:
;; recursivamente, comienza a buscar el valor almacenado de la variable desde
;; ambientes mas nuevos, moviendose hasta el mas viejo, terminando su busqueda al encontrar su valor
;; o en caso de no encontrarse, al llegar al ambiente vacio, arrojando un error de No binding.
;; para el caso de los ambientes recursivos, se construye su cerradura correspondiente al procedimiento
;; encontrado. Se usa para buscar valores almacenados en los ambientes del programa.

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
      (extendido-recursivo (proc-nombres idss cuerpos amb-viejo)
                           (let ((pos (list-find-position sym proc-nombres)))
                           (if (number? pos)
                               (cerradura (list-ref idss pos)
                                          (list-ref cuerpos pos)
                                          amb)
                               (buscar-variable amb-viejo sym))
                           ))
      )))


;****************************************************************************************
;Funciones Auxiliares
;*******************************************************************************************
; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

;; list-find-position
;; Este procedimiento recibe un simbolo y una lista de simbolos, tiene como funcion
;; buscar la posicion en la que aparece un simbolo dentro de una lista.
;; Este usa a list-index para comparar cada elemento de la lista con el simbolo recibido.
;; se usa para la busqueda de variables dentro de los ambientes. 
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))


;; list-index
;; Este procedimiento recibe un predicado y una lista,
;; su funcion es recorrer de manera recursiva la lista hasta encontrar el primer simbolo con el
;; cual se cumple el predicado dado, es decir, es el simbolo buscado. en caso de encontrarlo,
;; retorna su posicion dentro de esta lista. en caso de que no retorna un #f.
;; Se usa en list-find-position de manera auxiliar para realizar busquedas dentro de listas.
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

#|
***** PUNTO A **************************************************************
declarar* [

  @divEntera(@n) =
    Si (@n < 10) {
      0
    } sino {
      (1 + evaluar @divEntera((@n ~ 10)) finEval)
    }

  @sumarDigitos(@n) =
    Si (@n == 0) {
      0
    } sino {
      declarar(@cociente = evaluar @divEntera(@n) finEval;) {
        (evaluar @sumarDigitos(@cociente) finEval
         +
         (@n ~ (@cociente * 10)))
      }
    }

] {
  evaluar @sumarDigitos(147) finEval
}

***** PUNTO B **************************************************************
declarar*[
    @recursion(@x) = Si (@x == 1) {1}
                     sino {(evaluar @recursion((@x~1)) finEval * @x) }] {
evaluar @recursion(5) finEval
}


declarar*[
    @recursion(@x) = Si (@x == 1) {1}
                     sino {(evaluar @recursion((@x~1)) finEval * @x) }] {
evaluar @recursion(10) finEval
}

***** PUNTO C **************************************************************
declarar*[
    @potencia(@base, @exponente) = Si (@exponente == 0) {1}
                                  sino {(evaluar @potencia(@base, (@exponente~1)) finEval * @base) }] {
evaluar @potencia(2,4) finEval
}

***** PUNTO D **************************************************************
declarar*[
    @sumaRango(@inicio, @fin) = Si (@inicio == @fin) {@inicio}
                                  sino {(evaluar @sumaRango((@inicio + 1), @fin) finEval + @inicio) }] {
evaluar @sumaRango(2,5) finEval
}


***** PUNTO E **************************************************************
VERSION ANTIGUA:

declarar*[
    @integrantes() = "Santiago_JuanJosé_y_Sebastian"
    @saludar(@f) = ("Hola:" concat evaluar @f() finEval)
    @decorate() = evaluar @saludar (@integrantes) finEval
]{
    evaluar @decorate () finEval 
}

CORRECCION:

declarar(
    @integrantes = procedimiento()
                     {"Santiago"};
    @saludar = procedimiento(@f)
                     {procedimiento(){("Hola:" concat evaluar @f() finEval)}};
)
{
   declarar(
       @decorate = evaluar @saludar (@integrantes) finEval;
   )
   {
       evaluar @decorate() finEval
   }
}

***** PUNTO F **************************************************************
VERSION ANTIGUA:

declarar*[
    @integrantes() = "Santiago_JuanJosé_y_Sebastian"
    @saludar(@f) = ("Hola:" concat evaluar @f() finEval)
    @decorate(@mensaje) = (evaluar @saludar (@integrantes) finEval concat @mensaje)
]{
    evaluar @decorate ("_EstudiantesFLP") finEval 
}

CORRECCION

declarar(
    @integrantes = procedimiento()
                     {"Santiago"};
    @saludar = procedimiento(@f)
                     {procedimiento(@g){(("Hola:" concat evaluar @f() finEval) concat @g)}};
)
{
   declarar(
       @decorate = evaluar @saludar (@integrantes) finEval;
   )
   {
       evaluar @decorate("_estudiantes") finEval
   }
}
****************************************************************************
|#

(interpretador)