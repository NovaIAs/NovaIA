```scheme
;; Definición de función recursiva para calcular el factorial de un número.
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; Definición de función para calcular el máximo común divisor de dos números.
(define (mcd a b)
  (if (= b 0)
      a
      (mcd b (modulo a b))))

;; Definición de función para calcular el mínimo común múltiplo de dos números.
(define (mcm a b)
  (* a b) (/ (mcd a b)))

;; Definición de función para generar una lista de números primos hasta un límite dado.
(define (primos límite)
  (define (es-primo? n)
    (cond
      [(= n 2) true]
      [(zero? (modulo n 2)) false]
      [else
       (for/list ([i (in-range 3 (sqrt n) 2)])
         (zero? (modulo n i)))]))

  (let loop ([n 2] [primos '()])
    (if (> n límite) primos
        (if (es-primo? n)
            (loop (+ n 1) (cons n primos))
            (loop (+ n 1) primos)))))

;; Definición de función para calcular el valor absoluto de un número.
(define (valor-absoluto n)
  (if (> n 0) n (- 0 n)))

;; Función para calcular el signo de un número.
(define (signo n)
  (if (> n 0) 1
      (if (= n 0) 0 -1)))

;; Definición de función para ordenar una lista de números en orden ascendente.
(define (ordenar lista)
  (if (empty? lista)
      '()
      (let ([menor (car lista)] [resto (cdr lista)])
        (cons menor (ordenar (for/list ([x resto]) (> x menor) x))))))

;; Definición de función para invertir una lista.
(define (invertir lista)
  (for/list ([x lista]) x))

;; Definición de función para eliminar los elementos duplicados de una lista.
(define (eliminar-duplicados lista)
  (for/list ([x lista] [res '()]) (not (member x res)) (cons x res))))

;; Definición de función para encontrar el elemento mayor de una lista.
(define (mayor lista)
  (apply max lista))

;; Definición de función para encontrar el elemento menor de una lista.
(define (menor lista)
  (apply min lista))

;; Definición de función para calcular la suma de los elementos de una lista.
(define (suma lista)
  (apply + lista))

;; Definición de función para calcular el producto de los elementos de una lista.
(define (producto lista)
  (apply * lista))

;; Definición de función para calcular la media de los elementos de una lista.
(define (media lista)
  (/ (suma lista) (length lista)))

;; Definición de función para calcular la mediana de los elementos de una lista.
(define (mediana lista)
  (let ([ordenada (ordenar lista)])
    (if (even? (length ordenada))
        (/ (+ (car ordenada) (cadr ordenada)) 2) (caar ordenada))))

;; Definición de función para calcular la desviación estándar de los elementos de una lista.
(define (desv-estándar lista)
  (let ([media (media lista)] [suma (suma (for/list ([x lista]) (- x media) (* x x))))] sqrt (/ suma (- (length lista) 1))))

;; Definición de función para calcular la varianza de los elementos de una lista.
(define (varianza lista)
  (let ([suma (suma (for/list ([x lista]) (- x (media lista)) (* x x))))] (/ suma (- (length lista) 1))))
```

Explicación del código:

* La función `factorial` calcula el factorial de un número utilizando recursión.
* La función `mcd` calcula el máximo común divisor de dos números utilizando el algoritmo de Euclides.
* La función `mcm` calcula el mínimo común múltiplo de dos números utilizando el MCD y el MCM.
* La función `primos` genera una lista de números primos hasta un límite dado.
* La función `valor-absoluto` calcula el valor absoluto de un número.
* La función `signo` calcula el signo de un número.
* La función `ordenar` ordena una lista de números en orden ascendente.
* La función `invertir` invierte una lista.
* La función `eliminar-duplicados` elimina los elementos duplicados de una lista.
* La función `mayor` encuentra el elemento mayor de una lista.
* La función `menor` encuentra el elemento menor de una lista.
* La función `suma` calcula la suma de los elementos de una lista.
* La función `producto` calcula el producto de los elementos de una lista.
* La función `media` calcula la media de los elementos de una lista.
* La función `mediana` calcula la mediana de los elementos de una lista.
* La función `desv-estándar` calcula la desviación estándar de los elementos de una lista.
* La función `varianza` calcula la varianza de los elementos de una lista.