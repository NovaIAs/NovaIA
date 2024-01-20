```scheme
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (or (= n 0) (= n 1))
    n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (suma-cuadrados n)
  (let loop ((n n) (suma 0))
    (if (= n 0)
      suma
      (loop (- n 1) (+ suma (expt n 2))))))

(define (es-primo? n)
  (let loop ((divisor 2))
    (if (or (< n 2) (= divisor n))
      (= divisor n)
      (if (zero? (remainder n divisor))
        #f
        (loop (+ divisor 1))))))

(define (combinaciones k n)
  (let loop ((k k) (n n) (result 1))
    (if (= k 0)
      result
      (loop (- k 1) (- n 1) (* result n)))))

(define (permutaciones k n)
  (let loop ((k k) (n n) (result 1))
    (if (= k 0)
      result
      (loop (- k 1) (- n 1) (* result n))))))

(define (mayor-elemento lista)
  (define (max-aux lista max)
    (if (null? lista)
      max
      (if (> (car lista) max)
        (max-aux (cdr lista) (car lista))
        (max-aux (cdr lista) max))))
  (max-aux lista (car lista))))

(define (menor-elemento lista)
  (define (min-aux lista min)
    (if (null? lista)
      min
      (if (< (car lista) min)
        (min-aux (cdr lista) (car lista))
        (min-aux (cdr lista) min))))
  (min-aux lista (car lista))))

(define (ordenar lista)
  (if (null? lista)
    '()
    (cons (menor-elemento lista)
          (ordenar (remover (menor-elemento lista) lista)))))

(define (invertir lista)
  (let loop ((lista lista) (result '()))
    (if (null? lista)
      result
      (loop (cdr lista) (cons (car lista) result))))))

(define (eliminar-duplicados lista)
  (define (eliminar-duplicados-aux lista result)
    (if (null? lista)
      result
      (if (member (car lista) result)
        (eliminar-duplicados-aux (cdr lista) result)
        (eliminar-duplicados-aux (cdr lista) (cons (car lista) result)))))
  (eliminar-duplicados-aux lista '()))

(define (aplanar lista)
  (define (aplanar-aux lista result)
    (if (null? lista)
      result
      (if (list? (car lista))
        (aplanar-aux (append (car lista) (cdr lista)) result)
        (aplanar-aux (cdr lista) (cons (car lista) result)))))
  (aplanar-aux lista '()))

(define (rotar-derecha lista n)
  (append (sublist lista n (length lista))
          (sublist lista 0 n)))

(define (rotar-izquierda lista n)
  (append (sublist lista (- (length lista) n) (length lista))
          (sublist lista 0 (- (length lista) n))))

(define (sumar-listas lista1 lista2)
  (if (null? lista1)
    lista2
    (if (null? lista2)
      lista1
      (cons (+ (car lista1) (car lista2))
            (sumar-listas (cdr lista1) (cdr lista2))))))
```

Este código contiene una serie de funciones matemáticas y de procesamiento de listas en SCHEME. Algunas de las funciones incluidas son:

* `factorial`: Calcula el factorial de un número.
* `fibonacci`: Calcula el n-ésimo número de Fibonacci.
* `suma-cuadrados`: Calcula la suma de los cuadrados de los primeros n números naturales.
* `es-primo?`: Determina si un número es primo.
* `combinaciones`: Calcula el número de combinaciones de k elementos en un conjunto de n elementos.
* `permutaciones`: Calcula el número de permutaciones de k elementos en un conjunto de n elementos.
* `mayor-elemento`: Encuentra el elemento más grande en una lista.
* `menor-elemento`: Encuentra el elemento más pequeño en una lista.
* `ordenar`: Ordena una lista en orden ascendente.
* `invertir`: Invierte una lista.
* `eliminar-duplicados`: Elimina los elementos duplicados de una lista.
* `aplanar`: Aplana una lista de listas en una sola lista.
* `rotar-derecha`: Rota una lista n elementos hacia la derecha.
* `rotar-izquierda`: Rota una lista n elementos hacia la izquierda.
* `sumar-listas`: Suma dos listas de números.