```scheme

; Definir una función recursiva para calcular el factorial de un número.

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; Definir una función que devuelva el número de elementos de una lista.

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

; Definir una función que invierta el orden de los elementos de una lista.

(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst))
              (list (car lst)))))

; Definir una función que ordene una lista de números en orden ascendente.

(define (sort lst)
  (if (null? lst)
      '()
      (let ((min (car lst)))
        (append (sort (filter (lambda (x) (< x min)) (cdr lst)))
                (list min)
                (sort (filter (lambda (x) (>= x min)) (cdr lst)))))))

; Definir una función que calcule el producto de los elementos de una lista de números.

(define (product lst)
  (if (null? lst)
      1
      (* (car lst) (product (cdr lst)))))

; Definir una función que calcule la suma de los elementos de una lista de números.

(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

; Definir una función que calcule la media de los elementos de una lista de números.

(define (average lst)
  (/ (sum lst) (length lst)))

; Definir una función que calcule la varianza de los elementos de una lista de números.

(define (variance lst)
  (/ (sum (map (lambda (x) (expt (- x (average lst)) 2)) lst))
     (length lst)))

; Definir una función que calcule la desviación estándar de los elementos de una lista de números.

(define (standard-deviation lst)
  (sqrt (variance lst)))

```

Este código proporciona una serie de funciones útiles para trabajar con listas de números, incluyendo:

* Función factorial: Calcula el factorial de un número.
* Función length: Devuelve el número de elementos de una lista.
* Función reverse: Invierte el orden de los elementos de una lista.
* Función sort: Ordena una lista de números en orden ascendente.
* Función product: Calcula el producto de los elementos de una lista de números.
* Función sum: Calcula la suma de los elementos de una lista de números.
* Función average: Calcula la media de los elementos de una lista de números.
* Función variance: Calcula la varianza de los elementos de una lista de números.
* Función standard-deviation: Calcula la desviación estándar de los elementos de una lista de números.

Estas funciones pueden utilizarse para realizar una amplia variedad de cálculos y análisis de datos.