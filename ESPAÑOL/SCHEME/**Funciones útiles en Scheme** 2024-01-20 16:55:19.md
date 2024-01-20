```scheme
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (sub1 n)) (fibonacci (sub2 n)))))

(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (modulo a b))))

(define (is-prime? n)
  (if (< n 2)
      false
      (let loop ((i 2) (is-prime? true))
        (if (or (>= i (sqrt n)) is-prime?)
            is-prime?
            (if (zero? (modulo n i))
                false
                (loop (add1 i) is-prime?))))))

(define (quicksort xs)
  (if (null? xs)
      '()
      (let ((pivot (car xs)))
        (append (quicksort (filter (lambda (x) (< x pivot)) (cdr xs)))
                (cons pivot (quicksort (filter (lambda (x) (>= x pivot)) (cdr xs)))))))))

(define (merge xs ys)
  (if (or (null? xs) (null? ys))
      (append xs ys)
      (if (< (car xs) (car ys))
          (cons (car xs) (merge (cdr xs) ys))
          (cons (car ys) (merge xs (cdr ys))))))

(define (mergesort xs)
  (if (null? xs)
      '()
      (if (null? (cdr xs))
          xs
          (merge (mergesort (take xs (/ (length xs) 2)))
                 (mergesort (drop xs (/ (length xs) 2)))))))

(define (binary-search xs x)
  (let loop ((low 0) (high (- (length xs) 1)))
    (if (< low high)
        (let ((mid (/ (+ low high) 2)))
          (if (= (car xs mid) x)
              mid
              (if (< (car xs mid) x)
                  (loop mid high)
                  (loop low mid))))
        -1)))

(define (string-reverse s)
  (if (null? s)
      ""
      (string-append (string-reverse (cdr s)) (car s))))

(define (string-capitalize s)
  (if (null? s)
      ""
      (string-append (string-toupper (car s))
                     (string-capitalize (cdr s)))))

(define (string-contains? s x)
  (if (null? s)
      false
      (or (= (car s) x) (string-contains? (cdr s) x))))

(define (string-split s sep)
  (if (null? s)
      '()
      (cond [(string-contains? s sep)
             (cons (substring s 0 (string-index s sep))
                   (string-split (substring s (+ (string-index s sep) 1) (length s))
                                sep))]
            [else
             (list s)])))
```

Este código contiene 11 funciones:

1. `factorial`: Calcula el factorial de un número.
2. `fibonacci`: Calcula el número de Fibonacci en una posición dada.
3. `gcd`: Calcula el máximo común divisor de dos números.
4. `is-prime?`: Comprueba si un número es primo.
5. `quicksort`: Ordena una lista usando el algoritmo quicksort.
6. `merge`: Fusiona dos listas ordenadas en una sola lista ordenada.
7. `mergesort`: Ordena una lista usando el algoritmo mergesort.
8. `binary-search`: Busca un elemento en una lista ordenada usando el algoritmo de búsqueda binaria.
9. `string-reverse`: Invierte una cadena de caracteres.
10. `string-capitalize`: Capitaliza la primera letra de una cadena de caracteres.
11. `string-contains?`: Comprueba si una cadena de caracteres contiene otra cadena de caracteres.
12. `string-split`: Divide una cadena de caracteres en una lista de subcadenas, separadas por un separador especificado.