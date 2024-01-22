```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (suma-impares n)
  (if (= n 0)
      0
      (+ n (suma-impares (- n 2)))))

(define (es-primo n)
  (if (or (= n 0) (= n 1))
      False
      (and (>= n 2)
           (null? (filter (lambda (x) (= 0 (modulo n x)))
                           (range 2 (sqrt n)))))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define (mcd a b)
  (if (= b 0)
      a
      (mcd b (modulo a b))))

(define (mcm a b)
  (* (/ a (mcd a b)) b))

(define (ordenar lista)
  (if (null? (cdr lista))
      lista
      (cons (apply min lista)
            (ordenar (remove (lambda (x) (= x (apply min lista))) lista)))))

(define (combinaciones n k)
  (if (= k n)
      1
      (* n (combinaciones (- n 1) (- k 1)))))

(define (permutaciones n)
  (if (= n 0)
      1
      (* n (permutaciones (- n 1)))))

(define (potencia x n)
  (if (= n 0)
      1
      (* x (potencia x (- n 1)))))

(define (raiz-cuadrada n)
  (fixed-point (lambda (x) (/ (+ x (/ n x)) 2))
                1.0))

(define (area-triangulo base altura)
  (/ (* base altura) 2))

(define (area-circulo radio)
  (* pi (expt radio 2)))

(define (volumen-esfera radio)
  (* (4/3) pi (expt radio 3)))
```

Este código es una colección de funciones matemáticas comunes escritas en el lenguaje de programación Scheme. Las funciones incluyen factorial, suma de impares, comprobación de primos, cálculo de Fibonacci, máximo común divisor, mínimo común múltiplo, ordenación de listas, cálculo de combinaciones y permutaciones, cálculo de potencias y raíces cuadradas, y cálculo de áreas y volúmenes de figuras geométricas simples.

El código está bien documentado y sigue un estilo de programación funcional, utilizando funciones recursivas y estructuras de datos inmutables. También hace uso de la función `fixed-point` para calcular la raíz cuadrada de un número mediante el método de punto fijo.

Este código es un ejemplo de cómo crear un programa complejo y bien organizado en Scheme, utilizando una variedad de técnicas de programación funcional.