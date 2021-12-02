#lang racket
; Digito Verificador Rut versión Declarativa
; Programa que aplica el algoritmo del calculo del digito verificador de el rut de forma declarativa con funciones nativas del paradigma funcional

; Reduce: funcion que aplica un acumulador a los parametros aplicando una funcion
; Dominio: function X elem X list
; Recorrido: (function elem)
(define (reduce op acum lista)
  (if (empty? lista)
      acum
      (reduce op (op acum (car lista)) (cdr lista))))


; rutString->rutList: Función que convierte el rut "string" a una lista
; dominio: string
; recorrido: lista
(define (rutString->rutList rut)
  (cdr(reverse(cdr (map (lambda (digi)(string->number digi))(string-split rut ""))))))

(define (showResult digito)
  (if (eqv? 11 digito)
      0
      (if (eqv? 10 digito)
          #\K
          digito)))

; digitoVerificador: Funcíón que contiene el algoritmo a aplicar a un rut para obtener su digito verificador
; dominio: String
; recorrido: int
(define (digitoVerificador rut)
   (define (ruti rut)
     (define (encap rut cycle cycle2)
       (if (empty? rut)
           null
           (if (null? cycle2)
               (encap rut cycle cycle)
               (cons (* (car rut)(car cycle2))(encap (cdr rut)cycle(cdr cycle2))))))
     (encap rut(list 2 3 4 5 6 7)(list 2 3 4 5 6 7)))
   (showResult(- 11(- (reduce + 0(ruti (rutString->rutList rut))) (* 11 (quotient (reduce + 0(ruti (rutString->rutList rut))) 11))))))

; Ejemplos
(digitoVerificador "9975327")
(digitoVerificador "27395503")
(digitoVerificador "19997050")

