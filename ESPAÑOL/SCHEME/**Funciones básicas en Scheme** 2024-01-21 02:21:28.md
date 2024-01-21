```scheme
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (suma-digitos n)
  (if (= 0 n)
    0
    (+ (modulo n 10) (suma-digitos (quotient n 10)))))

(define (es-primo n)
  (define (es-primo-aux n divisor)
    (if (= divisor 1)
      #t
      (if (= 0 (modulo n divisor))
        #f
        (es-primo-aux n (sub1 divisor)))))

  (es-primo-aux n (- n 1)))

(define (mcd a b)
  (if (= b 0)
    a
    (mcd b (modulo a b))))

(define (mcm a b)
  (* a (/ b (mcd a b)))))

(define (es-perfecto n)
  (= n (suma-divisores-propios n)))

(define (suma-divisores-propios n)
  (if (= 0 n)
    0
    (+ (modulo n 2) (suma-divisores-propios (quotient n 2)))))

(define (fibonacci n)
  (if (= n 0)
    0
    (if (= n 1)
      1
      (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define (max-lista lista)
  (if (null? lista)
    0
    (if (> (car lista) (max-lista (cdr lista)))
      (car lista)
      (max-lista (cdr lista)))))

(define (min-lista lista)
  (if (null? lista)
    0
    (if (< (car lista) (min-lista (cdr lista)))
      (car lista)
      (min-lista (cdr lista)))))

(define (promedio-lista lista)
  (/ (suma-lista lista) (length lista)))

(define (suma-lista lista)
  (if (null? lista)
    0
    (+ (car lista) (suma-lista (cdr lista)))))

(define (invertir-lista lista)
  (if (null? lista)
    '()
    (append (invertir-lista (cdr lista)) (list (car lista)))))

(define (ordenar-lista lista)
  (if (null? lista)
    '()
    (let ((min (min-lista lista)))
      (cons min (ordenar-lista (remove min lista))))))

(define (concatenar-listas lista)
  (if (null? lista)
    '()
    (append (car lista) (concatenar-listas (cdr lista)))))
```

Explicación del código:

* La función `factorial` calcula el factorial de un número.
* La función `suma-digitos` calcula la suma de los dígitos de un número.
* La función `es-primo` comprueba si un número es primo.
* La función `mcd` calcula el máximo común divisor de dos números.
* La función `mcm` calcula el mínimo común múltiplo de dos números.
* La función `es-perfecto` comprueba si un número es perfecto.
* La función `suma-divisores-propios` calcula la suma de los divisores propios de un número.
* La función `fibonacci` calcula el n-ésimo número de Fibonacci.
* La función `max-lista` calcula el máximo valor de una lista.
* La función `min-lista` calcula el mínimo valor de una lista.
* La función `promedio-lista` calcula el promedio de los valores de una lista.
* La función `suma-lista` calcula la suma de los valores de una lista.
* La función `invertir-lista` invierte el orden de los elementos de una lista.
* La función `ordenar-lista` ordena los elementos de una lista en orden ascendente.
* La función `concatenar-listas` concatena todas las listas de una lista en una sola lista.