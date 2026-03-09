#lang eopl

(define coin-change
  (lambda (monto monedas)
    (cond
      [(< monto 0)    0]
      [(= monto 0)    1]
      [(null? monedas) 0]
      [else (+ (coin-change (- monto (car monedas)) monedas)
               (coin-change monto (cdr monedas)))])))