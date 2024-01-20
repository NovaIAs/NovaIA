```scheme

; Definición de una función recursiva para calcular el factorial de un número

(define (factorial n)
  (cond ((= n 0) 1)
        ((> n 0) (* n (factorial (- n 1))))
        (else (error "Número negativo"))))

; Definición de una función para calcular el área de un triángulo

(define (area-triángulo base altura)
  (* (/ base 2) altura))

; Definición de una función para calular el área de un círculo

(define (area-círculo radio)
  (* pi (* radio radio)))

; Definición de una función para calcular el área de un cuadrado

(define (area-cuadrado lado)
  (* lado lado))

; Definición de una función para calcular el área de un rectángulo

(define (area-rectángulo base altura)
  (* base altura))

; Definición de una función para calcular el área de un paralelogramo

(define (area-paralelogramo base altura)
  (* base altura))

; Definición de una función para calcular el área de un trapecio

(define (area-trapecio baseMayor baseMenor altura)
  (* (/ (+ baseMayor baseMenor) 2) altura))

; Definición de una función para calcular el área de un rombo

(define (area-rombo diagonalMayor diagonalMenor)
  (* (/ (* diagonalMayor diagonalMenor) 2) 2))

; Definición de una función para calcular el área de un hexágono regular

(define (area-hexagono-regular lado)
  (* 6 (* lado lado) sin(60)))

; Definición de una función para calcular el área de un octágono regular

(define (area-octágono-regular lado)
  (* 8 (* lado lado) sin(45)))

; Definición de una función para calcular el área de un decágono regular

(define (area-decágono-regular lado)
  (* 10 (* lado lado) sin(36)))

; Definición de una función para calcular el área de un dodecágono regular

(define (area-dodecágono-regular lado)
  (* 12 (* lado lado) sin(30)))

```

Explicación del código:

* La función `factorial` calcula el factorial de un número. Si el número es negativo, devuelve un error.

* La función `area-triángulo` calcula el área de un triángulo.

* La función `area-círculo` calcula el área de un círculo.

* La función `area-cuadrado` calcula el área de un cuadrado.

* La función `area-rectángulo` calcula el área de un rectángulo.

* La función `area-paralelogramo` calcula el área de un paralelogramo.

* La función `area-trapecio` calcula el área de un trapecio.

* La función `area-rombo` calcula el área de un rombo.

* La función `area-hexágono-regular` calcula el área de un hexágono regular.

* La función `area-octágono-regular` calcula el área de un octágono regular.

* La función `area-decágono-regular` calcula el área de un decágono regular.

* La función `area-dodecágono-regular` calcula el área de un dodecágono regular.