```scheme

(define (fib n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

(define (factorial n)
  (cond
    [(= n 0) 1]
    [else (* n (factorial (- n 1)))]))

(define (suma-lista lista)
  (cond
    [(= lista '()) 0]
    [else (+ (car lista) (suma-lista (cdr lista)))]))

(define (promedio lista)
  (/ (suma-lista lista) (length lista)))

(define (mayor-lista lista)
  (cond
    [(= lista '()) 0]
    [else (max (car lista) (mayor-lista (cdr lista)))]))

(define (menor-lista lista)
  (cond
    [(= lista '()) 0]
    [else (min (car lista) (menor-lista (cdr lista)))]))

(define (buscar elemento lista)
  (cond
    [(= lista '()) #f]
    [(= elemento (car lista)) #t]
    [else (buscar elemento (cdr lista))]))

(define (ordenar-lista lista)
  (cond
    [(= lista '()) '()]
    [else (cons (menor-lista lista) (ordenar-lista (filter (lambda (x) (> x (menor-lista lista))) lista)))]))

(define (invertir-lista lista)
  (cond
    [(= lista '()) '()]
    [else (cons (car lista) (invertir-lista (cdr lista)))]))

(define (es-palindromo palabra)
  (= palabra (invertir-lista palabra)))

```

Este código en SCHEME es una recopilación de diversas funciones útiles y ampliamente utilizadas. A continuación se detalla una breve explicación de cada una:

* `fib`: calcula el n-ésimo número de Fibonacci.
* `factorial`: calcula el factorial de un número entero positivo.
* `suma-lista`: suma todos los elementos de una lista.
* `promedio`: calcula el promedio de una lista de números.
* `mayor-lista`: encuentra el elemento más grande de una lista.
* `menor-lista`: encuentra el elemento más pequeño de una lista.
* `buscar`: busca un elemento en una lista y devuelve `#t` si lo encuentra o `#f` si no lo encuentra.
* `ordenar-lista`: ordena una lista de números en orden ascendente.
* `invertir-lista`: invierte el orden de los elementos de una lista.
* `es-palindromo`: comprueba si una palabra es un palíndromo (se lee igual de izquierda a derecha que de derecha a izquierda).