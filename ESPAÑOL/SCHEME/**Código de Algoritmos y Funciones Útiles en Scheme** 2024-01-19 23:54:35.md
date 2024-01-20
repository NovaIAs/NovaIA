```scheme
(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (or (= n 0) (= n 1))
    n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (suma-lista lista)
  (if (null? lista)
    0
    (+ (car lista) (suma-lista (cdr lista)))))

(define (mayor-elemento lista)
  (if (null? (cdr lista))
    (car lista)
    (max (car lista) (mayor-elemento (cdr lista)))))

(define (es-primo? n)
  (if (= n 2)
    true
    (and (not (par? n))
         (for*/list ((d (in-range 2 (sqrt n))))
           (not (= (modulo n d) 0))))))

(define (invertir-lista lista)
  (if (null? lista)
    null
    (append (invertir-lista (cdr lista)) (list (car lista)))))

(define (ordenar-lista lista)
  (if (null? lista)
    null
    (insert (car lista) (ordenar-lista (cdr lista)))))

(define (mergesort lista)
  (if (or (null? lista) (null? (cdr lista)))
    lista
    (let* ((mitad (length lista) / 2)
           (izquierda (sublist lista 0 mitad))
           (derecha (sublist lista mitad (length lista))))
      (merge (mergesort izquierda) (mergesort derecha)))))

(define (quicksort lista)
  (if (null? lista)
    null
    (let* ((pivote (car lista))
           (menores (remove pivote lista))
           (mayores (remove (lambda (x) (< x pivote)) menores)))
      (append (quicksort menores) (list pivote) (quicksort mayores))))))
```

Este código implementa una variedad de algoritmos y funciones útiles en Scheme. Incluye funciones para calcular el factorial de un número, la serie de Fibonacci, la suma de los elementos de una lista, el mayor elemento de una lista, comprobar si un número es primo, invertir una lista, ordenar una lista usando diferentes algoritmos y dividir una lista en sublistas.