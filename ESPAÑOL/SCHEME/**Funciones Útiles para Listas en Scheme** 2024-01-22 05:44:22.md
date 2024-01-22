```scheme
(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

(define (fib n)
  (if (or (= n 0) (= n 1))
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(define (primo? n)
  (define (aux n d)
    (if (= d 1)
      true
      (and (> n d) (aux n (- d 1)))))
  (if (<= n 1)
    false
    (aux n (- n 1))))

(define (mcd a b)
  (define (aux a b)
    (if (= b 0)
      a
      (aux b (% a b))))
  (aux a b))

(define (mcm a b)
  (* a b) (/ a (gcd a b)))

(define (suma-lista lista)
  (if (null? lista)
    0
    (+ (car lista) (suma-lista (cdr lista)))))

(define (max-lista lista)
  (define (aux lista max)
    (if (null? lista)
      max
      (if (> (car lista) max)
        (aux (cdr lista) (car lista))
        (aux (cdr lista) max))))
  (aux lista (car lista)))

(define (min-lista lista)
  (define (aux lista min)
    (if (null? lista)
      min
      (if (< (car lista) min)
        (aux (cdr lista) (car lista))
        (aux (cdr lista) min))))
  (aux lista (car lista)))

(define (ordenar-lista lista)
  (if (null? lista)
    '()
    (cons (min-lista lista) (ordenar-lista (remove (min-lista lista) lista)))))

(define (buscar-lista elemento lista)
  (define (aux elemento lista)
    (if (null? lista)
      false
      (or (= (car lista) elemento) (aux elemento (cdr lista)))))
  (aux elemento lista))

(define (eliminar-duplicados lista)
  (define (aux lista nueva-lista)
    (if (null? lista)
      nueva-lista
      (if (not (buscar-lista (car lista) nueva-lista))
        (aux (cdr lista) (cons (car lista) nueva-lista))
        (aux (cdr lista) nueva-lista))))
  (aux lista '()))

(define (invertir-lista lista)
  (define (aux lista nueva-lista)
    (if (null? lista)
      nueva-lista
      (aux (cdr lista) (cons (car lista) nueva-lista))))
  (aux lista '()))

(define (concatenar-listas lista1 lista2)
  (append lista1 lista2))

(define (particionar lista n)
  (define (aux lista n nueva-lista)
    (if (null? lista)
      nueva-lista
      (if (= n 0)
        (aux (cdr lista) n (cons (car lista) nueva-lista))
        (aux (cdr lista) (- n 1) (cons (car lista) nueva-lista))))))
  (aux lista n '()))

(define (mezclar-listas lista1 lista2)
  (define (aux lista1 lista2 mezclada)
    (if (null? lista1)
      mezclada
      (if (null? lista2)
        (aux lista1 lista2 (append mezclada lista1))
        (if (< (car lista1) (car lista2))
          (aux (cdr lista1) lista2 (cons (car lista1) mezclada))
          (aux lista1 (cdr lista2) (cons (car lista2) mezclada))))))
  (aux lista1 lista2 '()))

(define (quicksort lista)
  (define (aux lista pivote)
    (define (menor-pivote lista)
      (filter (lambda (x) (< x pivote)) lista))
    (define (mayor-pivote lista)
      (filter (lambda (x) (>= x pivote)) lista))
    (append (aux (menor-pivote lista) pivote) (cons pivote (aux (mayor-pivote lista) pivote))))
  (if (null? lista)
    '()
    (aux lista (car lista))))
```

**Explicación:**

Este código en Scheme es bastante complejo e implementa una variedad de funciones útiles para trabajar con datos en listas. Aquí hay una breve explicación de cada función:

* `factorial`: Calcula el factorial de un número entero.
* `fib`: Calcula el n-ésimo número de Fibonacci.
* `primo?`: Comprueba si un número entero es primo.
* `mcd`: Calcula el máximo común divisor de dos enteros.
* `mcm`: Calcula el mínimo común múltiplo de dos enteros.
* `suma-lista`: Calcula la suma de los elementos de una lista.
* `max-lista`: Encuentra el elemento máximo de una lista.
* `min-lista`: Encuentra el elemento mínimo de una lista.
* `ordenar-lista`: Ordena una lista en orden ascendente.
* `buscar-lista`: Comprueba si un elemento se encuentra en una lista.
* `eliminar-duplicados`: Elimina los elementos duplicados de una lista.
* `invertir-lista`: Invierte el orden de los elementos de una lista.
* `concatenar-listas`: Concatena dos listas en una sola.
* `particionar`: Divide una lista en dos partes iguales.
* `mezclar-listas`: Mezcla dos listas en una sola lista ordenada.
* `quicksort`: Ordena una lista usando el algoritmo Quicksort.

Estas funciones son todas muy útiles y se pueden usar para resolver una amplia variedad de problemas.