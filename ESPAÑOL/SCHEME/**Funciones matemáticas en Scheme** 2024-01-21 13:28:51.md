```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (suma-impares n)
  (if (= n 0)
      0
      (+ n (suma-impares (- n 2)))))

(define (es-primo n)
  (define (divisores-propios n)
    (filter (lambda (d) (= (modulo n d) 0))
            (iota (1 (- n 1)))))

  (= (length (divisores-propios n)) 0))

(define (lista-primos n)
  (filter es-primo
          (iota 2 n)))

(define (suma-primos n)
  (sum (lista-primos n)))

(define (es-perfecto n)
  (= n (suma-divisores-propios n)))

(define (lista-perfectos n)
  (filter es-perfecto
          (iota 1 n)))

(define (suma-perfectos n)
  (sum (lista-perfectos n)))

(define (es-abundante n)
  (> (suma-divisores-propios n) n))

(define (lista-abundantes n)
  (filter es-abundante
          (iota 1 n)))

(define (suma-abundantes n)
  (sum (lista-abundantes n)))

(define (es-deficiente n)
  (< (suma-divisores-propios n) n))

(define (lista-deficientes n)
  (filter es-deficiente
          (iota 1 n)))

(define (suma-deficientes n)
  (sum (lista-deficientes n)))
```

Explicación:

* `factorial`: Esta función calcula el factorial de un número. El factorial de un número `n` es el producto de todos los números naturales menores o iguales a `n`. Por ejemplo, `factorial(5) = 5 * 4 * 3 * 2 * 1 = 120`.
* `fibonacci`: Esta función calcula el número de Fibonacci de un número. Los números de Fibonacci son una serie de números en la que cada número es la suma de los dos números anteriores. Los primeros números de Fibonacci son 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ... Por ejemplo, `fibonacci(5) = 5`.
* `suma-impares`: Esta función calcula la suma de los números impares menores o iguales a un número dado. Por ejemplo, `suma-impares(5) = 1 + 3 + 5 = 9`.
* `es-primo`: Esta función determina si un número es primo o no. Un número primo es un número natural mayor que 1 que no tiene divisores positivos distintos de 1 y él mismo. Por ejemplo, `es-primo(5) = #t`, `es-primo(6) = #f`.
* `lista-primos`: Esta función genera una lista de los números primos menores o iguales a un número dado. Por ejemplo, `lista-primos(10) = (2 3 5 7)`.
* `suma-primos`: Esta función calcula la suma de los números primos menores o iguales a un número dado. Por ejemplo, `suma-primos(10) = 17`.
* `es-perfecto`: Esta función determina si un número es perfecto o no. Un número perfecto es un número natural que es igual a la suma de sus divisores propios. Por ejemplo, `es-perfecto(6) = #t`, `es-perfecto(7) = #f`.
* `lista-perfectos`: Esta función genera una lista de los números perfectos menores o iguales a un número dado. Por ejemplo, `lista-perfectos(10) = (6)`.
* `suma-perfectos`: Esta función calcula la suma de los números perfectos menores o iguales a un número dado. Por ejemplo, `suma-perfectos(10) = 6`.
* `es-abundante`: Esta función determina si un número es abundante o no. Un número abundante es un número natural que es mayor que la suma de sus divisores propios. Por ejemplo, `es-abundante(12) = #t`, `es-abundante(13) = #f`.
* `lista-abundantes`: Esta función genera una lista de los números abundantes menores o iguales a un número dado. Por ejemplo, `lista-abundantes(10) = (12)`.
* `suma-abundantes`: Esta función calcula la suma de los números abundantes menores o iguales a un número dado. Por ejemplo, `suma-abundantes(10) = 12`.
* `es-deficiente`: Esta función determina si un número es deficiente o no. Un número deficiente es un número natural que es menor que la suma de sus divisores propios. Por ejemplo, `es-deficiente(8) = #t`, `es-deficiente(9) = #f`.
* `lista-deficientes`: Esta función genera una lista de los números deficientes menores o iguales a un número dado. Por ejemplo, `lista-deficientes(10) = (1 2 4 5 7 8)`.
* `suma-deficientes`: Esta función calcula la suma de los números deficientes menores o iguales a un número dado. Por ejemplo, `suma-deficientes(10) = 27`.