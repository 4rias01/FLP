;; count-odd-and-even :
;; Proposito:
;; L -> (x,y) : Procedimiento que cuenta el numero de nodos pares e impares en un arbol binario dado, devuelve una tupla con el numero de nodos pares e impares respectivamente.
(define (count-odd-and-even arbol)
  (cond
    ;; Nuestro caso base, cuando el arbol es vacio
    [(empty? arbol)
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