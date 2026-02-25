#lang eopl

;Taller FLP: Punto 11*

(define zip
  (lambda (F L1 L2)
    (if (null? L1)
        '()
        (if (null? L2)
            '()
            (cons ( F (car L1) (car L2) ) (zip F (cdr L1) (cdr L2) ) ) ))))


;; Pruebas :