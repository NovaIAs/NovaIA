```scheme
; Definimos la función "factorial" que calcula el factorial de un número dado.
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; Definimos la función "fib" que calcula el n-ésimo número de Fibonacci.
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

; Definimos la función "gcd" que calcula el máximo común divisor de dos números dados.
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Definimos la función "lcm" que calcula el mínimo común múltiplo de dos números dados.
(define (lcm a b)
  (* a (/ b (gcd a b))))

; Definimos la función "prime?" que comprueba si un número dado es primo.
(define (prime? n)
  (if (<= n 1)
      #f
      (let loop ((i 2))
        (if (= i (sqrt n))
            #t
            (or (zero? (remainder n i))
                (loop (+ i 1)))))))

; Definimos la función "quicksort" que ordena una lista de números dada.
(define (quicksort xs)
  (if (null? xs)
      '()
      (let ((pivot (car xs))
            (xs (cdr xs)))
        (append
         (quicksort (filter (lambda (x) (< x pivot)) xs))
         (list pivot)
         (quicksort (filter (lambda (x) (>= x pivot)) xs))))))

; Definimos la función "merge-sort" que ordena una lista de números dada.
(define (merge-sort xs)
  (if (null? xs)
      '()
      (let ((mid (length xs) (/ 2))))
        (merge
         (merge-sort (take mid xs))
         (merge-sort (drop mid xs))))))

; Definimos la función "merge" que fusiona dos listas ordenadas dadas.
(define (merge xs ys)
  (if (null? xs)
      ys
      (if (null? ys)
          xs
          (if (< (car xs) (car ys))
              (cons (car xs) (merge (cdr xs) ys))
              (cons (car ys) (merge xs (cdr ys)))))))

; Definimos la función "binary-search" que busca un elemento dado en una lista ordenada.
(define (binary-search xs x)
  (define search
    (lambda (start end)
      (let ((mid (floor (/ (+ start end) 2))))
        (if (= (car xs mid) x)
            mid
            (if (< (car xs mid) x)
                (search (+ mid 1) end)
                (search start (- mid 1)))))))
  (search 0 (- (length xs) 1)))
```

Este código es una colección de funciones útiles en Scheme. Incluye funciones para calcular el factorial de un número, el n-ésimo número de Fibonacci, el máximo común divisor y el mínimo común múltiplo de dos números, comprobar si un número es primo, ordenar una lista de números usando los algoritmos quicksort y merge-sort, fusionar dos listas ordenadas y buscar un elemento en una lista ordenada usando el algoritmo de búsqueda binaria.