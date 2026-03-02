#lang eopl
;; Ejercicios del Taller 1 :
;; Juan Jose Rodriguez Lozano (202419084 - 3743)
;; Santiago Arias Rojas (202416285 - 3743)
;; Sebastian Calvo Carvajal (202419118 - 3743)


;; FUNCION AUXILIAR GENERAL
;; -> Se escribe la gramatica de la funcion aqui, pues mas que pertenecer a un solo ejercicio,
;;    esta funcion se usa en varios del taller.
;;
;; append (AUXILIAR):
;; Proposito :
;; L x L -> L' : Procedimiento que toma los elementos de dos listas de Simbolos L, y 
;; los junta en una nueva Lista L' que contiene todos los simbolos de la primer lista
;; seguidos de los de la segunda.
;;
;; <List> ::= ()
;;        ::= (<Scheme-value> <List>)

(define append
  (lambda (L1 L2)
    (if (null? L1)
        L2
        (cons (car L1) (append (cdr L1) L2) ) )))







;; Ejercicio 1.

;; cumplen (AUXILIAR) :
;; Proposito:
;; Par x P -> Bool : Procedimiento que recibe una pareja (lista de 2 elementos)
;; y un predicado P, retorna #t si AMBOS elementos de la pareja satisfacen
;; el predicado P, y #f en caso contrario.
;;
;; <Pair> ::= (<Scheme-value> <Scheme-value>)

(define cumplen
  (lambda (pareja predicado)
    (and (predicado (car pareja)) (predicado (cadr pareja)))))

;; Pruebas:
;; Caso ambos elementos cumplen el predicado
(cumplen '(2 4) even?) ;; Retorno => #t
;; Caso solo uno cumple el predicado
(cumplen '(2 3) even?) ;; Retorno => #f
;; Caso ninguno cumple el predicado
(cumplen '(1 3) even?) ;; Retorno => #f


;; cambiar (AUXILIAR) :
;; Proposito:
;; Par -> Par' : Procedimiento que recibe una pareja (lista de 2 elementos)
;; y retorna una nueva pareja Par' con los elementos en orden invertido,
;; es decir, el segundo elemento pasa a ser el primero y viceversa.
;;
;; <Pair> ::= (<Scheme-value> <Scheme-value>)

(define cambiar
  (lambda (pareja)
    (cons (cadr pareja)
          (cons (car pareja) '()))))

;; Pruebas:
;; Caso par de numeros
(cambiar '(3 2)) ;; Retorno => (2 3)
;; Caso par de simbolos
(cambiar '(a b)) ;; Retorno => (b a)


;; invert :
;; Proposito:
;; L x P -> L' : Procedimiento que recibe una lista L de pares (listas de
;; tamaño 2) y un predicado P. Retorna una nueva lista L' con los pares
;; invertidos (y, x) unicamente de aquellos pares en los que AMBOS elementos
;; satisfacen el predicado P. Los pares que no cumplen la condicion son
;; descartados.
;;
;; <List>     ::= ()
;;            ::= (<Pair> <List>)
;;
;; <Pair>     ::= (<Scheme-value> <Scheme-value>)
;;
;; <Pair-List> ::= ()
;;              ::= (<Pair> <Pair-List>)

(define invert
  (lambda (L P)
    (cond
      [(eqv? L empty) empty]
      [(cumplen (car L) P)
       (cons (cambiar (car L)) (invert (cdr L) P))]
      [else (invert (cdr L) P)])))

;; Pruebas:
;; Caso con even?
(invert '((3 2) (4 2) (1 5) (2 8)) even?) ;; Retorno => ((2 4) (8 2))
;; Caso donde ninguna pareja cumple el predicado
(invert '((6 9) (10 90) (82 7)) odd?) ;; Retorno => ()
;; Caso lista vacia
(invert '() even?) ;; Retorno => ()
;; Caso todos cumplen el predicado
(invert '((2 4) (6 8)) even?) ;; Retorno => ((4 2) (8 6))







;; Ejercicio 2.

;; down :
;; Proposito:
;; L -> L' : Procedimiento que recibe una lista L y retorna una nueva lista L'
;; donde cada elemento de L es envuelto en un nivel adicional de parentesis,
;; es decir, cada elemento x pasa a ser (x).
;;
;; <List> ::= ()
;;        ::= (<Scheme-value> <List>)

(define down
  (lambda (L)
    (cond
      [(eqv? L empty) empty]
      [else (cons (cons (car L) '()) (down (cdr L)))])))

;; Pruebas:
;; Caso con numeros
(down '(1 2 3)) ;; Retorno => ((1) (2) (3))
;; Caso con elementos ya anidados
(down '((una) (buena) (idea))) ;; Retorno => (((una)) ((buena)) ((idea)))
;; Caso con elemento complejo
(down '(un (objeto (mas)) complicado)) ;; Retorno => ((un) ((objeto (mas))) (complicado))
;; Caso lista vacia
(down '()) ;; Retorno => ()



;; Ejercicio 3.

;; list-set :
;; Proposito:
;; L x N x X x P -> L' : Procedimiento que recibe una lista L, un numero n
;; (indice desde cero), un elemento x y un predicado P. Retorna una lista L'
;; igual a L, pero con el elemento en la posicion n reemplazado por x,
;; unicamente si dicho elemento original satisface el predicado P. Si no
;; lo satisface, la lista se retorna sin cambios.
;;
;; <List> ::= ()
;;        ::= (<Scheme-value> <List>)

(define list-set
  (lambda (L n x P)
    (cond
      [(eqv? L empty) empty]
      [(= n 0)
       (if (P (car L))
           (cons x (cdr L))
           L)]
      [else
       (cons (car L)
             (list-set (cdr L) (- n 1) x P))])))

;; Pruebas:
;; Caso donde el elemento en posicion n cumple el predicado
(list-set '(5 8 7 6) 2 '(1 2) odd?) ;; Retorno => (5 8 (1 2) 6)
;; Caso donde el elemento en posicion n NO cumple el predicado
(list-set '(5 8 7 6) 2 '(1 2) even?) ;; Retorno => (5 8 7 6)
;; Caso posicion 0 con elemento que cumple
(list-set '(5 8 7 6) 0 '(1 5 10) odd?) ;; Retorno => ((1 5 10) 8 7 6)
;; Caso posicion 0 con elemento que NO cumple
(list-set '(5 8 7 6) 0 '(1 5 10) even?) ;; Retorno => (5 8 7 6)




;; Ejercicio 4.

;; filter-in :
;; Proposito:
;; P x L -> L' : Procedimiento que recibe un predicado P y una lista L.
;; Retorna una nueva lista L' que contiene unicamente los elementos de L
;; que satisfacen el predicado P, en el mismo orden en que aparecen.
;;
;; <List> ::= ()
;;        ::= (<Scheme-value> <List>)

(define filter-in
  (lambda (P L)
    (cond
      [(eqv? L empty) empty]
      [else (if (P (car L))
                (cons (car L) (filter-in P (cdr L)))
                (filter-in P (cdr L)))])))

;; Pruebas:
;; Caso filtrando numeros
(filter-in number? '(a 2 (1 3) b 7)) ;; Retorno => (2 7)
;; Caso filtrando simbolos
(filter-in symbol? '(a (b c) 17 foo)) ;; Retorno => (a foo)
;; Caso filtrando strings
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3))) ;; Retorno => ("univalle" "racket" "flp")
;; Caso lista vacia
(filter-in number? '()) ;; Retorno => ()




;; Ejercicio 5.

;; invertir-lista (AUXILIAR) :
;; Proposito:
;; L -> L' : Procedimiento que recibe una lista de elementos L y retorna
;; una nueva lista L' con los mismos elementos pero en orden invertido.
;;
;; <List> ::= ()
;;        ::= (<Scheme-value> <List>)

(define invertir-lista
  (lambda (L)
    (cond
      [(eqv? L empty) empty]
      [else (append (invertir-lista (cdr L)) (list (car L)))])))

;; Pruebas:
;; Caso lista de simbolos
(invertir-lista '(a b c)) ;; Retorno => (c b a)
;; Caso lista de un elemento
(invertir-lista '(x)) ;; Retorno => (x)
;; Caso lista vacia
(invertir-lista '()) ;; Retorno => ()


;; palindrome? :
;; Proposito:
;; L -> Bool : Procedimiento que recibe una lista de simbolos o caracteres
;; L (que representa una palabra) y retorna #t si la palabra es palindromo,
;; es decir, se lee igual de izquierda a derecha que de derecha a izquierda,
;; y #f en caso contrario.
;;
;; <List> ::= ()
;;        ::= (<Symbol> <List>)

(define palindrome?
  (lambda (palabra)
    (let ((p-inversa (invertir-lista palabra)))
      (equal? palabra p-inversa))))

;; Pruebas:
;; Caso palindromo clasico
(palindrome? '(r a d a r)) ;; Retorno => #t
;; Caso palindromo con letras repetidas
(palindrome? '(n e u q u e n)) ;; Retorno => #t
;; Caso que NO es palindromo
(palindrome? '(h o l a)) ;; Retorno => #f
;; Caso lista vacia (palindromo trivial)
(palindrome? '()) ;; Retorno => #t






;; Ejercicio 6.

;; swapper :
;; Proposito:
;; E1 x E2 x L -> L' : Procedimiento que recibe dos elementos E1 y E2 y
;; una lista L. Retorna una nueva lista L' igual a L, donde cada ocurrencia
;; de E1 es reemplazada por E2 y cada ocurrencia de E2 es reemplazada por E1.
;; Los demas elementos permanecen sin cambios.
;;
;; <List> ::= ()
;;        ::= (<Scheme-value> <List>)

(define swapper
  (lambda (E1 E2 L)
    (cond
      [(eqv? L empty) empty]
      [(eqv? (car L) E1) (cons E2 (swapper E1 E2 (cdr L)))]
      [(eqv? (car L) E2) (cons E1 (swapper E1 E2 (cdr L)))]
      [else (cons (car L) (swapper E1 E2 (cdr L)))])))

;; Pruebas:
;; Caso del enunciado basico
(swapper 'a 'd '(a b c d)) ;; Retorno => (d b c a)
;; Caso con multiples ocurrencias de ambos elementos
(swapper 'a 'd '(a d () c d)) ;; Retorno => (d a () c a)
;; Caso con muchas alternaciones
(swapper 'x 'y '(y y x y x y x x y)) ;; Retorno => (x x y x y x y y x)
;; Caso lista vacia
(swapper 'a 'b '()) ;; Retorno => ()




;; Ejercicio 7.

;; auxCombi (AUXILIAR) :
;; Proposito:
;; Sim x L -> L' : Procedimiento que construye una lista de pares L', combinando
;; el simbolo dado Sim con cada elemento de la lista de simbolos L.
;;
;; <List> ::= ()
;;        ::= (<Scheme-value> <List>)
;;
;; <Pair> ::= (<Scheme-value> <Scheme-value>)
;;
;; <Pair-List> ::= ()
;;             ::= (<Pair> <Pair-List>)

(define auxCombi
  (lambda (a L)
    (if (null? L)
        '()
        (cons (list a (car L)) (auxCombi a (cdr L) ) ) )))

;; Pruebas:
;; Caso lista vacia
(auxCombi 'a '()) ;; Retorno => ()
;; Caso simbolo con lista de un elemento
(auxCombi 'a '(b)) ;; Retorno => ((a b))
;; Caso simbolo con lista de multiples elementos
(auxCombi 'x '(1 2 3)) ;; Retorno => ((x 1) (x 2) (x 3))
;; Caso numero con lista de simbolos
(auxCombi 1 '(a b c)) ;; Retorno => ((1 a) (1 b) (1 c))

;; cartesian-product :
;; Proposito :
;; L x L -> LP : Procedimiento que recibe 2 listas de simbolos sin repeticion L 
;; y retorna una nueva lista de tuplas LP que representan el producto 
;; cartesiano entre ambas listas.
;; 
;; <List> ::= ()
;;         ::= (<Scheme-value> <List>)
;;
;; <Pair> ::= (<Scheme-value> <Scheme-value>)
;;
;; <Pair-List> ::= ()
;;             ::= (<Pair> <Pair-List>)

(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1)
        '()
        (if (null? L2)
            '()
            (append (auxCombi (car L1) L2) (cartesian-product (cdr L1) L2) ) ))))

;; Pruebas:
;; Caso dos listas con numeros:
(cartesian-product '(1 2) '(3 4)) ;; Retorno => ((1 3) (1 4) (2 3) (2 4))
;; Caso lista con simbolos:
(cartesian-product '(a b) '(x y z)) ;; Retorno => ((a x) (a y) (a z) (b x) (b y) (b z))
;; Caso lista vacia
(cartesian-product '() '(1 2 3)) ;; Retorno => ()
;; Caso simbolos + numeros
(cartesian-product '(a b c) '(1 2 3)) ;; REtorno => ((a 1) (a 2) (a 3) (b 1) (b 2) (b 3) (c 1) (c 2) (c 3))





;; Ejercicio 8.

;; mapping :
;; Proposito :
;; F x L x L -> LP : Este procedimiento recibe una funcion unaria sobre numeros F y dos listas de 
;; numeros L de igual tamaño. Esta funcion retorna una lista de pares (a,b) LP donde
;; el primer elemento pertenece a la primer lista, y el otro termino a la segunda,
;; en estos pares se CUMPLE la condicion de que F(a) = b.
;; 
;; <List> ::= ()
;;        ::= (<number> <List>)
;; 
;; <Pair> ::= (<number> <number>)
;;
;; <Pair-List> ::= ()
;;             ::= (<Pair> <Pair-List>)

(define mapping
  (lambda (F L1 L2)
    (if (null? L1)
        '()
        (if (null? L2)
            '()
            (if (= (F (car L1)) (car L2))
                (cons  (list (car L1)(car L2)) (mapping F (cdr L1) (cdr L2))  )
                (mapping F (cdr L1) (cdr L2) ) )))))

;; Pruebas:
;; Caso donde TODOS cumplen la condicion de F
(mapping (lambda (x) (* x 2)) '(1 2 3) '(2 4 6)) ;; Retorno => ((1 2) (2 4) (3 6))
;; Caso donde ALGUNOS cumplen la condicion de F
(mapping (lambda (x) (+ x 1)) '(1 2 3) '(2 0 4)) ;; Retorno => ((1 2) (3 4))
;; Caso donde NINGUNO cumple la condicion de F
(mapping (lambda (x) (* x 3)) '(1 2 3) '(1 2 3)) ;; Retorno => ()
;; Caso de lista vacia
(mapping (lambda (x) (* x 2)) '() '(2 4 6)) ;; Retorno => ()





;; Ejercicio 9.

;; sumAux (AUXILIAR):
;; Proposito :
;; L -> N : Procedimiento que recibe una lista de numeros sin repeticion L, la cual se
;; recorre en su totalidad, comparando el primer termino (car L) con el resto de
;; elementos de la lista, sumando 1 cada vez que la cabeza sea mayor, indicando
;; que esta invertido. Esta retornara el numero total de inversiones N presentes
;; en la lista respecto al primer termino de L.
;;
;; <List> ::= ()
;;        ::= (<number> <List>)

(define sumAux
  (lambda(L)
    (if (null? L)
        0
        (if (null? (cdr L))
            0
            (if (> (car L) (car (cdr L)) )
                (+ 1 (sumAux (cons (car L) (cdr(cdr L)) ) ))
                (sumAux (cons (car L) (cdr(cdr L)) ) ) ) ) )))

;; Pruebas:
;; Caso lista vacia
(sumAux '()) ;; Retorno => 0
;; Caso lista de un elemento (sin inversiones posibles)
(sumAux '(5)) ;; Retorno => 0
;; Caso cabeza mayor que todos los demas elementos
(sumAux '(5 1 2 3)) ;; Retorno => 3
;; Caso cabeza menor que todos (sin inversiones)
(sumAux '(1 2 3 4)) ;; Retorno => 0
;; Caso cabeza mayor que algunos
(sumAux '(3 1 4 2)) ;; Retorno => 2

;; inversions :
;; Proposito :
;; L -> N : Este procedimiento recibe una lista de numeros sin repeticion L, la funcion
;; recorre toda la lista de manera recursiva, pasandole cada elemento de esta a la funcion
;; auxiliar sumAux, para finalmente retornar el numero total de inversiones N.
;;
;; <List> ::= ()
;;        ::= (<number> <List>)

(define inversions
  (lambda (L)
    (if (null? L)
        0
        (+ (sumAux L) (inversions (cdr L)) )  )))

;; Pruebas :
;; Caso lista vacia
(inversions '()) ;; Retorno => 0
;; Caso lista ordenada ascendente (es decir, sin inversiones)
(inversions '(1 2 3 4 5)) ;; Retorno => 0
;; Caso lista ordenada descendiente (es decir, totalmente invertida)
(inversions '(5 4 3 2 1)) ;; Retorno => 10
;; Caso lista parcialmente invertida
(inversions '(3 1 4 2)) ;; Retorno => 3





;; Ejercicio 10.

;; auxParenth (AUXILIAR INTERNA):
;; Proposito :
;; L x N -> Bool : Este procedimiento recibe una lista de simbolos L de dos tipos
;; 'O (open) y 'C (close) asi como una variable entera de acumulacion N. Retorna un Booleano que 
;; sera #true si los parentesis estan balanceados (en ningun momento hay mas C que O
;; en la lista), y #false en el caso contrario o en que los datos de entrada no sean correctos.
;;
;; <List> ::= ()
;;        ::= (<Symbol> <List>)
;;
;; balanced-parentheses? :
;; Proposito :
;; L -> Bool : Este procedimiento recibe una lista de simbolos de tipo 'O y 'C, y empleando
;; la funcion auxiliar auxParenth (debido a que no podemos modificar los parametros de entrada
;; y si o si necesitamos un acumulador), la cual nos retorna el valor Booleano.
;;
;; <List> ::= ()
;;        ::= (<Symbol> <List>)

(define balanced-parentheses?
  (lambda (L)
    (letrec
        ( (auxParenth
          (lambda (list acum)
            (cond
              ( (< acum 0) #f ) 
              ( (null? list) (= acum 0) ) 
              ( (eqv? (car list) 'O ) (auxParenth (cdr list) (+ acum 1)))
              ( (eqv? (car list) 'C ) (auxParenth (cdr list) (- acum 1)))    
              (else #f)))) )
      (auxParenth L 0))))

;; Pruebas :
;; Caso lista vacia
(balanced-parentheses? '()) ;; Retorno => #t
;; Caso de aceptacipn
(balanced-parentheses? '(O O C O C C)) ;; Retorno => #t
;; Caso de desbalanceo por primer criterio (queda abierto)
(balanced-parentheses? '(O O C)) ;; Retorno => #f
;; Caso de desbalanceo por segundo criterio (se cierra prematuramente)
(balanced-parentheses? '(O C C O)) ;; Retorno => #f





;; Ejercicio 11.

;; zip
;; Proposito :
;; F x L x L -> L' : Este prodecimiento recibe como entrada una funcion Binaria F, y dos listas
;; de numeros de igual tamaño L. Esta retorna una nueva lista, donde cada n-esimo elemento, es el
;; resultado de la operacion de la funcion Binaria F con los n-esimos elementos de las listas L.
;;
;; <List> ::= ()
;;        ::= (<number> <List>)

(define zip
  (lambda (F L1 L2)
    (if (null? L1)
        '()
        (if (null? L2)
            '()
            (cons ( F (car L1) (car L2) ) (zip F (cdr L1) (cdr L2) ) ) ))))

;; Pruebas:
;; Caso base listas vacias
(zip + '() '()) ;; Retorno => ()
;; Caso de suma
(zip + '(1 2 3) '(4 5 6)) ;; Retorno => (5 7 9)
;; Caso de producto
(zip * '(2 3 4) '(5 6 7)) ;; Retorno => (10 18 28)
;; Caso complejo usando funcion anonima como funcion binaria F
(zip (lambda (x y) (* (- x y) x)) '(10 20 30) '(1 2 3)) ;; Retorno => (90 360 810)





;; Ejercicio 12.

;; filter-acum :
;; Proposito :
;; n x n x F x acc x Fl -> acc' : Este procedimiento recibe dos numeros n, una funcion binaria F, un
;; valor inicial acumulador acc y una funcion unaria Fl. Este aplicara la funcio binaria F a todos los
;; numeros dentro del intervalo [a , b] que a su vez cumplan con el predicado de la funcion unaria Fl,
;; almacenando cada uno de estos valores en acc, retornandolo finalmente el valor total acc'.

(define filter-acum
  (lambda (a b F acum filter)
    (if (> a b)
        acum
        (if (equal? (filter a) #t)
            (filter-acum (+ a 1) b F (+ acum (F a)) filter)
            (filter-acum (+ a 1) b F (+ acum 0) filter) ))))

;; Pruebas :
;;Caso base (a>b)
(filter-acum 5 3 (lambda (x) x) 0 (lambda (x) #t)) ;; Retorno => 0
;; Caso sumar todos los numeros del intervalo
(filter-acum 1 5 (lambda (x) x) 0 (lambda (x) #t)) ;; Retorno => 15
;;Caso sumar solo los pares
(filter-acum 1 6 (lambda (x) x) 0 even?) ;; Retonor => 12
;; Caso aplicando cuadrado a impares
(filter-acum 1 5 (lambda (x) (* x x)) 0 odd?) ;; Retorno => 35





;; Ejercicio 13.

;; ultimo (AUXILIAR) :
;; Proposito :
;; L -> N : Este procedimiento auxiliar toma una lista de elementos L y retorna
;; el ULTIMO elemento n de la lista L, retornando vacio en caso de que la lista no tenga elementos.
;;
;; <List> ::= ()
;;        ::= (<Scheme-value> <List>)

(define ultimo
  (lambda (lista)
    (cond
      [(null? lista) '()]
      [(null? (cdr lista)) (car lista)]
      [else (ultimo (cdr lista))])))

;; Pruebas:
;; Caso lista vacia
(ultimo '()) ;; Retorno => ()
;; Caso lista de un elemento
(ultimo '(1)) ;; Retorno => 1
;; Caso lista de multiples elementos numericos
(ultimo '(1 2 3 4)) ;; Retorno => 4
;; Caso lista de simbolos
(ultimo '(a b c)) ;; Retorno => c

;; lst-sin-ultimo (AUXILIAR) :
;; Proposito :
;; L -> L' : Este procedimiendo auxiliar toma de entrada una lista de elementos L y
;; retorna una nueva lista L', que contiene todos los elementos de L MENOS el ultimo elemento.
;; Es importante señalar que esta lista no maneja el caso de lista vacia puesto que la funcion 
;; fue hecha asumiendo que las entradas sean dos listas de longitud mayor o igual a 1
;;
;; <List> ::= ()
;;        ::= (<Scheme-value> <List>)

(define lst-sin-ultimo
  (lambda (lista)
    (cond
      [(eqv? (cdr lista) empty) empty]
      [else (cons (car lista) (lst-sin-ultimo (cdr lista)))])
    )
  )

;; Pruebas:
;; Caso lista de un elemento
(lst-sin-ultimo '(1)) ;; Retorno => ()
;; Caso lista de dos elementos
(lst-sin-ultimo '(1 2)) ;; Retorno => (1)
;; Caso lista de multiples elementos numericos
(lst-sin-ultimo '(1 2 3 4)) ;; Retorno => (1 2 3)
;; Caso lista de simbolos
(lst-sin-ultimo '(a b c d)) ;; Retorno => (a b c)

;; operate : 
;; Proposito :
;; L x L -> N : Este procedimiento recibe dos listas L. la primera, sera una lista de
;; Operadores binarios de tamaño n, y la segunda, una lista de numeros de tamaño n + 1.
;; la funcion retornara el resultado de aplicar las operaciones de la primer lista
;; sucesivamente en los numeros de la segunda lista, retornando el valor acumulado N.
;;
;; <List> ::= ()
;;        ::= (<Scheme-value> <List>)

(define operate
  (lambda (lrators lrands)
    (cond
      [(null? (cddr lrands)) ((car lrators) (car lrands) (cadr lrands))]
      [else ((ultimo lrators) (operate (lst-sin-ultimo lrators) (lst-sin-ultimo lrands)) (ultimo lrands))])
    )
  )

;; Pruebas :
;; Caso minimo valido (1 operador y 2 numeeos)
(operate (list +) (list 3 4)) ;; Retorno => 7
;; 3 operadores distintos:
(operate (list + - *) (list 10 5 2 1)) ;; Retorno => 13
;; Resta negativa
(operate (list - - -) (list 1 5 12 16)) ;; Retorno => -32
;; Solo productos (factorial)
(operate (list * * * *) (list 5 4 3 2 1)) ;; Retorno => 120





;; Ejercicio 14

;; path:
;; Proposito:
;; number x <arbol-binario> -> (listof symbol) : dado un número y un árbol binario, devuelve la lista de direcciones ('left o 'right) que conducen al nodo que contiene el número; devuelve #f si no existe.
;;
;; <arbol-binario> ::= empty
;;                 ::= (number <arbol-binario> <arbol-binario>)

(define (path n BST)
  (cond
    [(null? BST) #f]                         ; si el arbol es vacio, no encontramos el numero en esa ruta, devolvemos falso
    [(= (car BST) n) '()]                    ; si lo encontramos, devolvemos una lista vacia :3
    [else                                     ; si no es ninguna de las 2 opciones empezamos el caso inductivo
     (let ([left-path  (path n (cadr BST))]) ; definimos left-path que va a ser una llamada recursiva con path en el nodo izquierdo
       (if left-path                          ; si no encontramos el #f, significa que encontramos el numero en esa ruta
           (cons 'left left-path)             ; aqui devolvemos la ruta con left al inicio si encontramos el numero en el nodo izquierdo
           (let ([right-path (path n (caddr BST))]) ; sino lo encontramos, entonces hacemos el llamado desde el nodo derecho
             (if right-path                          ; mismo check que con el izquierdo
                 (cons 'right right-path)            ; si no tenemos #f entonces devolvemos con el right al inicio
                 #f))))]))                           ; esto en teoria no deberia pasar porque los arboles deben tener el numero, pero por si acaso, devolvemos #f si no lo encontramos
;; Pruebas:
;; elemento en raíz
(path 5 '(5 (3 () ()) (4 () ()))) ;; => '( )
;; elemento en subárbol izquierdo
(path 3 '(5 (3 () ()) (4 () ()))) ;; => '(left)
;; elemento en subárbol derecho profundo
(path 4 '(5 (3 () ()) (6 (2 (8 () ()) (4)) ()))) ;; => '(right left right)
;; no existe en el árbol
(path 42 '(5 (3 () ()) (4 () ()))) ;; => #f




;; Ejercicio 15

;; count-odd-and-even:
;; Proposito:
;; <arbol-binario> -> (list number number) : dado un árbol binario de números, retorna la pareja (pares impares) con la cantidad de nodos pares e impares.
;;
;; <arbol-binario> ::= empty
;;                 ::= (number <arbol-binario> <arbol-binario>)

(define (count-odd-and-even arbol)
  (cond
    ;; Nuestro caso base, cuando el arbol es vacio
    [(null? arbol)
     (list 0 0)] 

    ;; Si no es vacio, llamamos recursivamente en los nodos izquierdo y derecho, sumamos los resultados y dependiendo del valor del nodo actual sumamos 1 en par o impar :D
    [else
     (let* ([valor (car arbol)]
            [izq   (cadr arbol)]
            [der   (caddr arbol)]
            [res-izq (count-odd-and-even izq)]
            [res-der (count-odd-and-even der)])
       (if (even? valor)
           (list (+ (car res-izq) (car res-der) 1)
                 (+ (cadr res-izq) (cadr res-der)))
           (list (+ (car res-izq) (car res-der))
                 (+ (cadr res-izq) (cadr res-der) 1))))]))

;; Pruebas:
(count-odd-and-even '()) ;; => (0 0)
(count-odd-and-even '(2 () ())) ;; => (1 0)
(count-odd-and-even '(1 (2 () ()) (3 () ()))) ;; => (1 2)
(count-odd-and-even '(4 (5 () ()) (6 () (7 () ())))) ;; => (2 2)





;; Ejercicio 16

;; hanoi:
;; Proposito:
;; number x symbol x symbol x symbol -> (listof (list symbol symbol)) : recibe el número de discos n y los nombres de las tres torres A, B, C; devuelve la lista de movimientos necesarios para trasladar la pila de A a C usando B como auxiliar. Cada movimiento es una pareja (from to).
;;
;; No se utiliza ninguna estructura recursiva adicional más allá de números y símbolos.

(define (hanoi n origen auxiliar destino)
  (if (= n 1)
      (list (list origen destino))
      (append
       (append (hanoi (- n 1) origen destino auxiliar) (list (list origen destino)))
       (hanoi (- n 1) auxiliar origen destino))))

;; Pruebas:
(hanoi 1 'A 'B 'C) ;; => '((A C))
(hanoi 2 'A 'B 'C) ;; => '((A B) (A C) (B C))
(hanoi 3 'A 'B 'C) ;; => '((A C) (A B) (C B) (A C) (B A) (B C) (A C))
(hanoi 4 'A 'B 'C) ;; movimientos esperados de 15 pasos





;; Ejercicio 17

;; coin-change:
;; Proposito:
;; number x (listof number) -> number : dado un monto y una lista de monedas (valores positivos), retorna el número de maneras de cambiar el monto usando las monedas disponibles (puede usar cada moneda varias veces).
;;
;; <List> ::= ()
;;        ::= (<number> <List>)

(define coin-change
  (lambda (monto monedas)
    (cond
      [(< monto 0)    0]
      [(= monto 0)    1]
      [(null? monedas) 0]
      [else (+ (coin-change (- monto (car monedas)) monedas)
               (coin-change monto (cdr monedas)))])))

;; Pruebas:
(coin-change 0 '(1 2 5)) ;; => 1
(coin-change 5 '(1 2 5)) ;; => 4
(coin-change 3 '(2))    ;; => 0
(coin-change 5 '())     ;; => 0





;; Ejercicio 18

;; sumar-listas (AUXILIAR):
;; Proposito:
;; (listof number) x (listof number) -> (listof number) : suma elemento a elemento dos listas de números; si una lista termina antes, se detiene.
;;
;; <List> ::= ()
;;        ::= (<number> <List>)

(define sumar-listas
  (lambda (L1 L2)
    (cond
      [(or (eqv? L1 empty) (eqv? L2 empty)) empty]
      [else (cons (+ (car L1) (car L2)) (sumar-listas (cdr L1) (cdr L2)))]))
  )

;; Pruebas:
(sumar-listas '(1 2 3) '(4 5 6)) ;; => (5 7 9)
(sumar-listas '() '(1 2))        ;; => ()
(sumar-listas '(1 2) '())        ;; => ()
(sumar-listas '(5 5) '(1 2 3))   ;; => (6 7)

;; pascal :
;; Proposito:
;; number -> (listof number) : devuelve la fila N del triángulo de Pascal (1-indexada).
;;
;; <List> ::= ()
;;        ::= (<number> <List>)

(define pascal
  (lambda (N)
    (if (= N 1)
        '(1)
        (letrec ([fila-prev (pascal (- N 1))]
                 [fila1 (cons 0 fila-prev)]
                 [fila2 (append fila-prev '(0))])
          (sumar-listas fila1 fila2)))))

;; Pruebas:
(pascal 1) ;; => (1)
(pascal 2) ;; => (1 1)
(pascal 3) ;; => (1 2 1)
(pascal 5) ;; => (1 4 6 4 1)


