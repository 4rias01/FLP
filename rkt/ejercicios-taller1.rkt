#lang eopl
;; Ejercicios del Taller 1 :
;; Juan Jose Rodriguez Lozano (202419084 - 3743)
;; Santiago Arias Rojas (202416285 - 3743)
;; Sebastian Calvo Carvajal (202419118 - 3743)


;; Ejercicio 1.

(define cumplen
  (lambda (pareja predicado)
    (and (predicado (car pareja)) (predicado (cadr pareja)))
    )
  )

(define cambiar
  (lambda (pareja)
    (cons (cadr pareja)
          (cons (car pareja) '()))))

(define invert
  (lambda (L P)
    (cond
      [(eqv? L empty) empty]
      [(cumplen (car L) P)
       (cons (cambiar (car L)) (invert (cdr L) P))]
      [else (invert (cdr L) P)]
      )
    )
  )





;; Ejercicio 2.

(define down
  (lambda (L)
    (cond
      [(eqv? L empty) empty]
      [else (cons (cons (car L) '()) (down (cdr L)))]
      )
    )
  )





;; Ejercicio 3.

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
             (list-set (cdr L) (- n 1) x P))])
    )
  )





;; Ejercicio 4.

(define filter-in
  (lambda (P L)
    (cond
      [(eqv? L empty) empty]
      [else (if (P (car L))
                (cons (car L) (filter-in P (cdr L)))
                (filter-in P (cdr L))
                )])
    )
  )





;; Ejercicio 5.

(define unir-listas
  (lambda (L1 L2)
    (cond
      [(eqv? L1 empty) L2]
      [else (cons (car L1) (unir-listas (cdr L1) L2))]))
  )

(define invertir-lista
  (lambda (L)
    (cond
      [(eqv? L empty) empty]
      [else (unir-listas (invertir-lista (cdr L)) (list (car L)))]
      ))
  )

(define palindrome?
  (lambda (palabra)
    (let
        (
         (p-inversa (invertir-lista palabra))
         )
      (equal? palabra p-inversa)
      )
    )
  )





;; Ejercicio 6.

(define swapper
  (lambda (E1 E2 L)
    (cond
      [(eqv? L empty) empty]
      [(eqv? (car L) E1) (cons E2
                               (swapper E1 E2 (cdr L)))]
      [(eqv? (car L) E2) (cons E1
                               (swapper E1 E2 (cdr L)))]
      [else (cons (car L)
                  (swapper E1 E2 (cdr L))
                  )])
    )
  )





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

;; auxParenth (AUXILIAR):
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
;;

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

(define ultimo
  (lambda (lista)
    (cond
      [(null? (cdr lista)) (car lista)]
      [else (ultimo (cdr lista))])))

(define lst-sin-ultimo
  (lambda (lista)
    (cond
      [(eqv? (cdr lista) empty) empty]
      [else (cons (car lista) (lst-sin-ultimo (cdr lista)))])
    )
  )

(define operate
  (lambda (lrators lrands)
    (cond
      [(null? (cddr lrands)) ((car lrators) (car lrands) (cadr lrands))]
      [else ((ultimo lrators) (operate (lst-sin-ultimo lrators) (lst-sin-ultimo lrands)) (ultimo lrands))])
    )
  )





;; Ejercicio 14.

;; path :
;; Proposito:
;; L -> L : Procedimiento que encuentra la ruta para llegar a un numero dado en un arbol binario, devuelve una lista con la ruta a seguir.
(define (path n tree)
  (cond
    [(null? tree) #f]                         ; si el arbol es vacio, no encontramos el numero en esa ruta, devolvemos falso
    [(= (car tree) n) '()]                    ; si lo encontramos, devolvemos una lista vacia :3
    [else                                     ; si no es ninguna de las 2 opciones empezamos el caso inductivo
     (let ([left-path  (path n (cadr tree))]) ; definimos left-path que va a ser una llamada recursiva con path en el nodo izquierdo
       (if left-path                          ; si no encontramos el #f, significa que encontramos el numero en esa ruta
           (cons 'left left-path)             ; aqui devolvemos la ruta con left al inicio si encontramos el numero en el nodo izquierdo
           (let ([right-path (path n (caddr tree))]) ; sino lo encontramos, entonces hacemos el llamado desde el nodo derecho
             (if right-path                          ; mismo check que con el izquierdo
                 (cons 'right right-path)            ; si no tenemos #f entonces devolvemos con el right al inicio
                 #f))))]))                           ; esto en teoria no deberia pasar porque los arboles deben tener el numero, pero por si acaso, devolvemos #f si no lo encontramos





;; Ejercicio 15.

;; count-odd-and-even :
;; Proposito:
;; L -> (x,y) : Procedimiento que cuenta el numero de nodos pares e impares en un arbol binario dado, devuelve una tupla con el numero de nodos pares e impares respectivamente.
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




;; Ejercicio 16.

;; hanoi :
;; Proposito:
;; L -> L : Procedimiento que resuelve el problema de las torres de hanoi, recibe el numero de discos y los nombres de las torres, devuelve una lista con los movimientos necesarios para resolver el problema.
(define (hanoi n A B C)
  (if (= n 1)
      (list (list A C))
      (append
       (hanoi (- n 1) A C B)
       (list (list A C))B
       (hanoi (- n 1) B A C))))




;; Ejercicio 17.

(define coin-change
  (lambda (monto monedas)
    (cond
      [(< monto 0)    0]
      [(= monto 0)    1]
      [(null? monedas) 0]
      [else (+ (coin-change (- monto (car monedas)) monedas)
               (coin-change monto (cdr monedas)))])))




;; Ejercicio 18.


(define sumar-listas
  (lambda (L1 L2)
    (cond
      [(or (eqv? L1 empty) (eqv? L2 empty)) empty]
      [else (cons (+ (car L1) (car L2)) (sumar-listas (cdr L1) (cdr L2)))]))
  )

(define pascal
  (lambda (N)
    (if (= N 1)
        '(1)
        (letrec ([fila-prev (pascal (- N 1))]
                 [fila1 (cons 0 fila-prev)]
                 [fila2 (unir-listas fila-prev '(0))])
          (sumar-listas fila1 fila2)))))





