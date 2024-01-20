```scheme
; Definición de las funciones

(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))

(define (suma n)
  (if (zero? n)
      0
      (+ n (suma (sub1 n)))))

(define (potencia x n)
  (if (zero? n)
      1
      (* x (potencia x (sub1 n)))))

; Ejemplo de uso

(display (factorial 5)) ; Muestra el resultado de calcular el factorial de 5
(newline)
(display (suma 10)) ; Muestra el resultado de calcular la suma de los números del 1 al 10
(newline)
(display (potencia 2 3)) ; Muestra el resultado de calcular la potencia de 2 elevado a 3

```

Explicación del código:

- La función `factorial` calcula el factorial de un número dado.

- La función `suma` calcula la suma de los números desde 1 hasta un número dado.

- La función `potencia` calcula la potencia de un número dado elevado a un exponente dado.

- El código de ejemplo utiliza las tres funciones para calcular el factorial de 5, la suma de los números del 1 al 10 y la potencia de 2 elevado a 3, y muestra los resultados en la consola.