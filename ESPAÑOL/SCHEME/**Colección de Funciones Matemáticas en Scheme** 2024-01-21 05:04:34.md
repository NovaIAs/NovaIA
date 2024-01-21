```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (suma-cuadrados n)
  (if (= n 0)
      0
      (+ (expt n 2) (suma-cuadrados (- n 1)))))

(define (maximo a b)
  (if (> a b)
      a
      b))

(define (minimo a b)
  (if (< a b)
      a
      b))

(define (ordenar lista)
  (if (null? lista)
      '()
      (cons (minimo lista (cdr lista)) (ordenar (remove (minimo lista (cdr lista)) lista)))))

(define (invertir lista)
  (cond
    ((null? lista) '())
    ((null? (cdr lista)) (list (car lista)))
    (else (cons (car lista) (invertir (cdr lista))))))


(define (suma-digitos n)
  (cond
    ((< n 10) n)
    (else (+ (modulo n 10) (suma-digitos (quotient n 10))))))

(define (es-primo? n)
  (cond
    ((<= n 1) #f)
    ((= n 2) #t)
    (else (every? (lambda (d) (not (= (modulo n d) 0))) (range 3 (sqrt n))))))

(define (mcd a b)
  (cond
    ((= b 0) a)
    (else (mcd b (modulo a b)))))

(define (mcm a b)
  (* a b) (quotient (/ a (mcd a b)))))
```

Este código contiene una variedad de funciones útiles y matemáticamente interesantes.

* `factorial`: Calcula el factorial de un número.
* `fibonacci`: Calcula el n-ésimo número de Fibonacci.
* `suma-cuadrados`: Calcula la suma de los cuadrados de los primeros n números enteros.
* `maximo` y `minimo`: Encuentra el máximo y el mínimo de dos números, respectivamente.
* `ordenar`: Ordena una lista de números en orden ascendente.
* `invertir`: Invierte una lista.
* `suma-digitos`: Calcula la suma de los dígitos de un número entero.
* `es-primo?`: Determina si un número es primo.
* `mcd`: Calcula el máximo común divisor de dos números.
* `mcm`: Calcula el mínimo común múltiplo de dos números.

Cada función está definida de manera concisa y clara, utilizando las características de Scheme, como funciones anidadas, recursión y programación funcional. El código es fácil de entender y modificar, lo que lo hace útil para una variedad de tareas matemáticas y de programación.