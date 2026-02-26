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
