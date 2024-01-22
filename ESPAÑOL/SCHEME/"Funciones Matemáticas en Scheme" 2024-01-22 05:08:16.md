```scheme
; Definición de funciones

(define (suma a b)
  (+ a b))

(define (resta a b)
  (- a b))

(define (multiplicacion a b)
  (* a b))

(define (division a b)
  (/ a b))

(define (modulo a b)
  (modulo a b))

(define (potencia a b)
  (expt a b))

(define (raiz-cuadrada a)
  (sqrt a))

(define (seno a)
  (sin a))

(define (coseno a)
  (cos a))

(define (tangente a)
  (tan a))

(define (logaritmo-neperiano a)
  (log a))

(define (logaritmo-decimal a)
  (log10 a))

; Uso de las funciones

(display "Suma de 3 y 5: ")
(display (suma 3 5))
(newline)

(display "Resta de 10 y 2: ")
(display (resta 10 2))
(newline)

(display "Multiplicación de 2 y 4: ")
(display (multiplicacion 2 4))
(newline)

(display "División de 12 y 3: ")
(display (division 12 3))
(newline)

(display "Módulo de 11 y 5: ")
(display (modulo 11 5))
(newline)

(display "Potencia de 2 elevado a 3: ")
(display (potencia 2 3))
(newline)

(display "Raíz cuadrada de 9: ")
(display (raiz-cuadrada 9))
(newline)

(display "Seno de 30 grados: ")
(display (seno (/ pi 6)))
(newline)

(display "Coseno de 45 grados: ")
(display (coseno (/ pi 4)))
(newline)

(display "Tangente de 60 grados: ")
(display (tangente (/ pi 3)))
(newline)

(display "Logaritmo neperiano de 10: ")
(display (logaritmo-neperiano 10))
(newline)

(display "Logaritmo decimal de 100: ")
(display (logaritmo-decimal 100))
(newline)
```

Este código está escrito en Scheme y contiene una serie de definiciones de funciones matemáticas básicas, como suma, resta, multiplicación, división, módulo, potencia, raíz cuadrada, seno, coseno, tangente, logaritmo neperiano y logaritmo decimal.

Después de definir estas funciones, el código utiliza la función `display` para mostrar los resultados de llamar a estas funciones con diferentes argumentos. Por ejemplo, el código muestra la suma de 3 y 5, la resta de 10 y 2, la multiplicación de 2 y 4, la división de 12 y 3, el módulo de 11 y 5, la potencia de 2 elevado a 3, la raíz cuadrada de 9, el seno de 30 grados, el coseno de 45 grados, la tangente de 60 grados, el logaritmo neperiano de 10 y el logaritmo decimal de 100.

Este código es un ejemplo de cómo se pueden definir y utilizar funciones en Scheme para realizar cálculos matemáticos.