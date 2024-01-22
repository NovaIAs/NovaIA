```scheme
(define (factorial n)
  (cond
    [(= n 0) 1]
    [else (* n (factorial (- n 1)))]))

(define (fibonacci n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))]))

(define (mayor-comun-divisor a b)
  (cond
    [(= b 0) a]
    [else (mayor-comun-divisor b (modulo a b))]))

(define (es-primo n)
  (cond
    [(= n 2) #t]
    [(= n 1) #f]
    [(= n 0) #f]
    [else (not (ormap (lambda (i) (= 0 (modulo n i))) (range 2 (sqrt n))))]))

(define (ordenar lista)
  (sort lista <))

(define (suma lista)
  (fold + 0 lista))

(define (producto lista)
  (fold * 1 lista))

(define (media lista)
  (/ (suma lista) (length lista)))

(define (mediana lista)
  (let* [(ordenada (ordenar lista)]
        [(largo (length ordenada))])
    (cond
      [(= largo 1) (car ordenada)]
      [(even? largo) (/ (+ (nth ordenada (- largo 1)) (nth ordenada (- largo 2))) 2)]
      [else (nth ordenada (/ (- largo 1) 2))])))

(define (desviacion-estandar lista)
  (sqrt (media (map square (map (- lista (media lista)) lista)))))

(define (varianza lista)
  (square (desviacion-estandar lista)))

(define (covarianza lista1 lista2)
  (media (map * (map (- lista1 (media lista1)) lista1) (map (- lista2 (media lista2)) lista2))))

(define (correlacion lista1 lista2)
  (/ (covarianza lista1 lista2) (* (desviacion-estandar lista1) (desviacion-estandar lista2)))))
```

Este código define una serie de funciones matemáticas comunes en Scheme:

* `factorial`: Calcula el factorial de un número.
* `fibonacci`: Calcula el n-ésimo número de Fibonacci.
* `mayor-comun-divisor`: Calcula el máximo común divisor de dos números.
* `es-primo`: Comprueba si un número es primo.
* `ordenar`: Ordena una lista de números en orden ascendente.
* `suma`: Calcula la suma de los números de una lista.
* `producto`: Calcula el producto de los números de una lista.
* `media`: Calcula la media de los números de una lista.
* `mediana`: Calcula la mediana de los números de una lista.
* `desviacion-estandar`: Calcula la desviación estándar de los números de una lista.
* `varianza`: Calcula la varianza de los números de una lista.
* `covarianza`: Calcula la covarianza de dos listas de números.
* `correlacion`: Calcula la correlación de dos listas de números.