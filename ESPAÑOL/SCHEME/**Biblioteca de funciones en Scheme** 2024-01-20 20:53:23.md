```scheme

;; FUNCIONES DE LISTAS

;; (define (primero lista) (car lista)) ; Retorna el primer elemento de una lista
;; (define (resto lista) (cdr lista)) ; Retorna el resto de una lista sin el primer elemento
;; (define (longitud lista) (length lista)) ; Retorna la longitud de una lista
;; (define (lista? lista) (list? lista)) ; Devuelve #t si lista es una lista, #f en caso contrario
;; (define (append lista1 lista2) (append lista1 lista2)) ; Concatena dos listas
;; (define (reverse lista) (reverse lista)) ; Invierte una lista

;; FUNCIONES NUMÉRICAS

;; (define (+ num1 num2) (+ num1 num2)) ; Suma dos números
;; (define (- num1 num2) (- num1 num2)) ; Resta dos números
;; (define (* num1 num2) (* num1 num2)) ; Multiplica dos números
;; (define (/ num1 num2) (/ num1 num2)) ; Divide dos números
;; (define (abs num) (abs num)) ; Devuelve el valor absoluto de un número
;; (define (max num1 num2) (max num1 num2)) ; Devuelve el máximo de dos números
;; (define (min num1 num2) (min num1 num2)) ; Devuelve el mínimo de dos números
;; (define (round num) (round num)) ; Redondea un número al entero más cercano

;; FUNCIONES LÓGICAS

;; (define (and cond1 cond2 ...) (and cond1 cond2 ...)) ; Devuelve #t si todas las condiciones son verdaderas, #f en caso contrario
;; (define (or cond1 cond2 ...) (or cond1 cond2 ...)) ; Devuelve #t si alguna de las condiciones es verdadera, #f en caso contrario
;; (define (not cond) (not cond)) ; Invierte el valor de una condición

;; FUNCIONES DE COMPARACIÓN

;; (define (= num1 num2) (= num1 num2)) ; Compara dos números para igualdad
;; (define (> num1 num2) (> num1 num2)) ; Compara dos números para mayor que
;; (define (< num1 num2) (< num1 num2)) ; Compara dos números para menor que
;; (define (>= num1 num2) (>= num1 num2)) ; Compara dos números para mayor o igual que
;; (define (<= num1 num2) (<= num1 num2)) ; Compara dos números para menor o igual que

;; FUNCIONES DE ENTRADA Y SALIDA

;; (define (print obj) (display obj)) ; Imprime un objeto en la consola
;; (define (input) (read)) ; Lee una línea de texto de la consola

;; FUNCIONES DE CONTROL DE FLUJO

;; (define (if cond conseq alt) (if cond conseq alt)) ; Ejecuta una rama u otra dependiendo de una condición
;; (define (cond ((cond1 conseq1) (cond2 conseq2) ...)) (cond ((cond1 conseq1) (cond2 conseq2) ...))) ; Ejecuta la primera rama cuyo condición sea verdadera
;; (define (while cond expr ...) (while cond expr ...)) ; Ejecuta un código mientras se cumpla una condición
;; (define (for var init fin expr ...) (for var init fin expr ...)) ; Ejecuta un código para cada elemento de un rango

;; FUNCIONES DE ALTO NIVEL

;; (define (map fun lista) (map fun lista)) ; Aplica una función a cada elemento de una lista
;; (define (filter pred lista) (filter pred lista)) ; Filtra una lista según un predicado
;; (define (foldr fun init lista) (foldr fun init lista)) ; Pliega una lista desde la derecha
;; (define (foldl fun init lista) (foldl fun init lista)) ; Pliega una lista desde la izquierda

```

Este código es una colección de funciones útiles en Scheme, organizadas en varias categorías:

* Funciones de listas: para trabajar con listas, como obtener el primer elemento, el resto de la lista, la longitud, etc.
* Funciones numéricas: para realizar operaciones aritméticas básicas, como sumar, restar, multiplicar, dividir, etc.
* Funciones lógicas: para evaluar condiciones, como "y", "o" y "no".
* Funciones de comparación: para comparar dos valores, como igualdad, mayor que, menor que, etc.
* Funciones de entrada y salida: para leer y escribir datos en la consola.
* Funciones de control de flujo: para controlar el flujo de ejecución del programa, como condicionales, bucles y declaraciones de salto.
* Funciones de alto nivel: para realizar operaciones más complejas, como mapear una función a una lista, filtrar una lista según un predicado o plegar una lista.

Este código se puede utilizar como una referencia para las funciones integradas en Scheme, o como una base para crear nuevos programas.