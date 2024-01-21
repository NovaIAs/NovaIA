```scheme
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (< n 2)
    n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (es-primo? n)
  (if (= n 1)
    false
    (let loop ((divisor 2))
      (if (= divisor n)
        true
        (if (= (remainder n divisor) 0)
          false
          (loop (+ divisor 1)))))))

(define (mcd a b)
  (if (= b 0)
    a
    (mcd b (remainder a b))))

(define (mcm a b)
  (/ (* a b) (mcd a b))))

(define (ordenar lista)
  (if (null? lista)
    '()
    (let* ((elemento (car lista))
           (resto (cdr lista)))
      (cons (min elemento (ordenar resto))
            (ordenar (filter (lambda (x) (> x elemento)) resto))))))

(define (invertir lista)
  (if (null? lista)
    '()
    (cons (car (cdr lista)) (invertir (car lista)))))

(define (suma-lista lista)
  (if (null? lista)
    0
    (+ (car lista) (suma-lista (cdr lista)))))

(define (promedio lista)
  (/ (suma-lista lista) (length lista))))

(define (mediana lista)
  (car (ordenar lista)))

(define (moda lista)
  (let* ((frecuencias (map (lambda (x) (cons x 0)) lista))
         (max-frecuencia (apply max (map car frecuencias))))
    (filter (lambda (x) (= (car x) max-frecuencia)) frecuencias)))

(define (varianza lista)
  (let* ((media (promedio lista))
         (desviaciones (map (lambda (x) (- x media)) lista)))
    (promedio (map (lambda (x) (square x)) desviaciones)))))

(define (desviacion-estandar lista)
  (sqrt (varianza lista))))
```

Este código es una colección de funciones matemáticas comunes en Scheme. Incluye funciones para calcular el factorial, el número de Fibonacci, la primalidad, el máximo común divisor, el mínimo común múltiplo, la ordenación, la inversión, la suma, el promedio, la mediana, la moda, la varianza y la desviación estándar.

Las funciones están escritas en un estilo funcional, utilizando una combinación de recursión y construcciones de listas. Se utilizan muchas funciones de orden superior, como `map`, `filter` y `reduce`.

Este código es un buen ejemplo de la potencia y expresividad del lenguaje Scheme.