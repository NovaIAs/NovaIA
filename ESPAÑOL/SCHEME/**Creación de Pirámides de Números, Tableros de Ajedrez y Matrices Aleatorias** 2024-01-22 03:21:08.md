```scheme
;; Se define una función recursiva para calcular el factorial de un número.
(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

;; Se define una función para imprimir una pirámide de números.
(define (imprimir_piramide n)
  (for ((i (in-range n)))
    (for ((j (in-range (- n i))))
      (display " "))
    (for ((k (in-range i)))
      (display (factorial k))
      (display " "))
    (display "\n")))

;; Se define una función para imprimir un tablero de ajedrez.
(define (imprimir_tablero n)
  (for ((i (in-range n)))
    (for ((j (in-range n)))
      (if (even? (+ i j))
        (display "#")
        (display " "))
      (display " ")))

;; Se define una función para imprimir una matriz.
(define (imprimir_matriz m)
  (for ((i (in-range (length m))))
    (for ((j (in-range (length (m i)))))
      (display (m i j))
      (display " "))
    (display "\n")))

;; Se define una función para generar una matriz de números aleatorios.
(define (generar_matriz n m)
  (for ((i (in-range n)))
    (for ((j (in-range m)))
      (random))))

;; Se define una función para imprimir una matriz de números aleatorios.
(define (imprimir_matriz_aleatoria n m)
  (imprimir_matriz (generar_matriz n m)))

;; Se imprime una pirámide de números.
(imprimir_piramide 5)

;; Se imprime un tablero de ajedrez.
(imprimir_tablero 8)

;; Se imprime una matriz de números aleatorios.
(imprimir_matriz_aleatoria 5 5)
```

Explicación del código:

* La función `factorial` calcula el factorial de un número.

* La función `imprimir_piramide` imprime una pirámide de números.

* La función `imprimir_tablero` imprime un tablero de ajedrez.

* La función `imprimir_matriz` imprime una matriz.

* La función `generar_matriz` genera una matriz de números aleatorios.

* La función `imprimir_matriz_aleatoria` imprime una matriz de números aleatorios.

* La última línea del código imprime una pirámide de números, un tablero de ajedrez y una matriz de números aleatorios.