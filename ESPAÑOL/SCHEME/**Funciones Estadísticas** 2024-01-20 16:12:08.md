```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define (lista-primos n)
  (define (es-primo? n)
    (if (= n 1)
        false
        (for/list (i (in-range 2 n))
            (not (= (modulo n i) 0)))))

  (for/list (i (in-range 1 n))
      (if (es-primo? i) i)))

(define (suma-cuadrados n)
  (for/sum (i (in-range 1 n))
      (* i i)))

(define (promedio n)
  (/ (suma-cuadrados n) n))

(define (mediana n)
  (sort n)
  (if (odd? n)
      (list-ref n (/ n 2))
      (average (list-ref n (/ n 2)) (list-ref n (- (/ n 2) 1)))))

(define (desviacion-estandar n)
  (define (varianza n)
    (/ (suma-cuadrados n) (- n 1))))

  (sqrt (varianza n)))

(define (coeficiente-variacion n)
  (/ (desviacion-estandar n) (promedio n))))

(define (imprimir-estadisticas n)
  (display "Factorial: ")
  (displayln (factorial n))
  (display "Fibonacci: ")
  (displayln (fibonacci n))
  (display "Lista de Primos: ")
  (displayln (lista-primos n))
  (display "Suma de Cuadrados: ")
  (displayln (suma-cuadrados n))
  (display "Promedio: ")
  (displayln (promedio n))
  (display "Mediana: ")
  (displayln (mediana n))
  (display "Desviación Estándar: ")
  (displayln (desviacion-estandar n))
  (display "Coeficiente de Variación: ")
  (displayln (coeficiente-variacion n)))

(imprimir-estadisticas 10)

```

Explicación:

Este código define varias funciones para realizar cálculos estadísticos sobre un número entero `n`:

1. `factorial`: Calcula el factorial de `n`.
2. `fibonacci`: Calcula el n-ésimo número de Fibonacci.
3. `lista-primos`: Devuelve una lista con los números primos hasta `n`.
4. `suma-cuadrados`: Calcula la suma de los cuadrados de los números enteros de 1 a `n`.
5. `promedio`: Calcula el promedio de los números enteros de 1 a `n`.
6. `mediana`: Calcula la mediana de los números enteros de 1 a `n`.
7. `desviacion-estandar`: Calcula la desviación estándar de los números enteros de 1 a `n`.
8. `coeficiente-variacion`: Calcula el coeficiente de variación de los números enteros de 1 a `n`.
9. `imprimir-estadisticas`: Esta función llama a todas las funciones anteriores y muestra los resultados en la consola.

Finalmente, se llama a la función `imprimir-estadisticas` con un valor de `n` igual a 10, lo que muestra todos los resultados de los cálculos estadísticos para los números enteros de 1 a 10.