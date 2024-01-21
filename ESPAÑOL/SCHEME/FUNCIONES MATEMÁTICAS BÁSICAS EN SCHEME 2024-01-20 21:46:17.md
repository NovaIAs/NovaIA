```scheme

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (es_primo n)
  (if (<= n 1)
      #f
      (for ([i (in-range 2 (sqrt n))])
        (not (= (modulo n i) 0)))))

(define (suma_primos n)
  (for/list ([i (in-range 1 n)])
    (when (es_primo i)
      i)))

(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (suma_fibonacci n)
  (for/list ([i (in-range 1 n)])
    (fibonacci i)))

(define (es_par n)
  (= (modulo n 2) 0))

(define (suma_pares n)
  (for/list ([i (in-range 1 n)])
    (when (es_par i)
      i)))

(display (factorial 5))
(displayln "")
(display (es_primo 7))
(displayln "")
(display (suma_primos 10))
(displayln "")
(display (fibonacci 10))
(displayln "")
(display (suma_fibonacci 10))
(displayln "")
(display (es_par 10))
(displayln "")
(display (suma_pares 10))

; Explicación del código:
;
; 1. Función factorial: Calcula el factorial de un número entero positivo. El factorial de n es el producto de todos los números enteros positivos desde 1 hasta n. Por ejemplo, factorial(5) = 5 * 4 * 3 * 2 * 1 = 120.
;
; 2. Función es_primo: Determina si un número entero positivo es primo o no. Un número primo es un número entero positivo mayor que 1 que solo es divisible por 1 y por sí mismo. Por ejemplo, 7 es primo, mientras que 10 no lo es.
;
; 3. Función suma_primos: Calcula la suma de todos los números primos menores o iguales a un número entero positivo dado. Por ejemplo, suma_primos(10) = 2 + 3 + 5 + 7 = 17.
;
; 4. Función fibonacci: Calcula el n-ésimo número de Fibonacci. Los números de Fibonacci son una serie de números en la que cada número es la suma de los dos números anteriores. Los primeros números de Fibonacci son: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ... Por ejemplo, fibonacci(10) = 55.
;
; 5. Función suma_fibonacci: Calcula la suma de los n primeros números de Fibonacci. Por ejemplo, suma_fibonacci(10) = 0 + 1 + 1 + 2 + 3 + 5 + 8 + 13 + 21 + 34 = 89.
;
; 6. Función es_par: Determina si un número entero es par o no. Un número par es un número entero que es divisible por 2. Por ejemplo, 10 es par, mientras que 7 no lo es.
;
; 7. Función suma_pares: Calcula la suma de todos los números pares menores o iguales a un número entero positivo dado. Por ejemplo, suma_pares(10) = 2 + 4 + 6 + 8 + 10 = 30.

```