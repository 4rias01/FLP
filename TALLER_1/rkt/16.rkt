;; hanoi :
;; Proposito:
;; L -> L : Procedimiento que resuelve el problema de las torres de hanoi, recibe el numero de discos y los nombres de las torres, devuelve una lista con los movimientos necesarios para resolver el problema.
(define (hanoi n A B C)
  (if (= n 1)
      (list (list A C))
      (append
       (hanoi (- n 1) A C B)
       (list (list A C))P
       (hanoi (- n 1) B A C))))
