```scheme
(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

(define (suma-digitos n)
  (if (< n 10)
    n
    (+ (modulo n 10) (suma-digitos (/ n 10)))))

(define (mcd a b)
  (if (= b 0)
    a
    (mcd b (modulo a b))))

(define (mcm a b)
  (* (/ (* a b) (mcd a b)) (mcd a b)))

(define (fibonacci n)
  (if (= n 0)
    0
    (if (= n 1)
      1
      (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define (primo? n)
  (for/list ([i 2 (+ i 1)])
    (< i (sqrt n))
    (not (= (modulo n i) 0))))

(define (solucion-ecuacion-cuadratica a b c)
  (values (- (+ b (sqrt (- (* b b) (* 4 a c)))) (/ 2 a))
          (- (+ b (sqrt (- (* b b) (* 4 a c)))) (/ 2 a))))

(define (area-triangulo b h)
  (/ (* b h) 2))

(define (perimetro-triangulo a b c)
  (+ a b c))

(define (area-circulo r)
  (* pi (* r r)))

(define (perimetro-circulo r)
  (* 2 pi r))

(define (volumen-esfera r)
  (* 4 (/ 3 pi) (* r r r)))

(define (superficie-esfera r)
  (* 4 pi (* r r)))

(define (volumen-cilindro r h)
  (* pi (* r r) h))

(define (superficie-cilindro r h)
  (+ (* 2 pi r h) (* 2 pi (* r r)))))
```

Este código proporciona un conjunto de funciones matemáticas en Scheme, incluyendo cálculo de factoriales, suma de dígitos, máximo común divisor, mínimo común múltiplo, serie de Fibonacci, comprobación de primalidad, solución de ecuaciones cuadráticas, cálculo de áreas y perímetros de figuras geométricas básicas, y cálculo de volúmenes y superficies de figuras geométricas tridimensionales.