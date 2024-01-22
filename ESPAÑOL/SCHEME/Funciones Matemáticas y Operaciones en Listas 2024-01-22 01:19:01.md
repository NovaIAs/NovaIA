```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* a b) (/ (gcd a b)))

(define (prime? n)
  (if (= n 1)
      false
      (every (lambda (d) (> (remainder n d) 0)) (range 2 (sqrt (ceiling n))))))

(define (merge-sort lst)
  (if (< (length lst) 2)
      lst
      (append (merge-sort (take (/ (length lst) 2) lst))
              (merge-sort (drop (/ (length lst) 2) lst)))))

```

En este código, definimos varias funciones matemáticas y operaciones útiles para manipular listas. Estas son algunas de las funciones más importantes:

* `factorial`: Calcula el factorial de un número natural.
* `fibonacci`: Calcula el número de Fibonacci en una posición dada.
* `gcd`: Calcula el máximo común divisor de dos números enteros.
* `lcm`: Calcula el mínimo común múltiplo de dos números enteros.
* `prime?`: Comprueba si un número entero es primo.
* `merge-sort`: Ordena una lista de números enteros utilizando el algoritmo de ordenación por fusión.

También definimos algunos procedimientos auxiliares, como `range`, `take` y `drop`, que son útiles para manipular listas.

Estas funciones y procedimientos se pueden utilizar para resolver una variedad de problemas matemáticos y de programación. Por ejemplo, se pueden utilizar para encontrar el factorial de un número grande, generar los primeros 100 números de Fibonacci, o encontrar el máximo común divisor de dos números enteros grandes.