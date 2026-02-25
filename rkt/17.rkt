#lang eopl

(define (my-length lst)
  (if (null? lst)
      0
      (+ 1 (my-length (cdr lst)))))

(define coin-change
  (lambda (monto monedas)
    (cond
      [(< monto 0) 0]
      [(= monto 0) 1]
      [(= (my-length monedas) 1) (coin-change
                                  (- monto (car monedas)) monedas)]
      [else (+ (coin-change (- monto (car monedas)) monedas)
               (coin-change monto (cdr monedas)))]))
  )