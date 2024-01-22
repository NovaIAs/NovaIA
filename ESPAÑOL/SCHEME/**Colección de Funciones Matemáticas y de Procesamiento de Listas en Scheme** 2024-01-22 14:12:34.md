```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (es-primo? n)
  (let loop ((d 2) (res #t))
    (if (or (= n 0) (>= d n))
        res
        (and res
             (not (= (mod n d) 0))
             (loop (+ d 1) res)))))

(define (es-palindromo? xs)
  (let interp ((i 0) (j (- (length xs) 1)))
    (if (>= i j)
        #t
        (and (char=? (string-ref xs i) (string-ref xs j))
             (interp (+ i 1) (- j 1))))))

(define (merge xs ys)
  (let loop ((res '()) (x xs) (y ys))
    (cond
      ((null? x) (append res y))
      ((null? y) (append res x))
      ((< (car x) (car y)) (loop (cons (car x) res) (cdr x) y))
      (else (loop (cons (car y) res) x (cdr y))))))

(define (quicksort xs)
  (let rec partition ((xs) (pivot (car xs)) (left '()) (right '()))
    (cond
      ((null? xs) (cons left right))
      ((< (car xs) pivot) (partition (cdr xs) pivot (cons (car xs) left) right))
      ((> (car xs) pivot) (partition (cdr xs) pivot left (cons (car xs) right)))
      (else (partition (cdr xs) pivot left right))))
  (let ((p (partition xs (car xs))))
    (append
      (quicksort (cdr (car p)))
      (cons (car (car p))
            (quicksort (cdr (cdr p)))))))

(define (heap-sort xs)
  (let rec build-max-heap ((xs) (parent 0) (left 1) (right 2))
    (cond
      ((>= parent (length xs)) xs)
      ((and (> (car xs) (car (cdr (cdr xs)))) (<= right (length xs))) (build-max-heap xs (max-index parent) left right))
      (else (build-max-heap (swap xs parent left) parent (max-index parent) right))))
  (let rec heap-sort-i ((xs) (i (length xs)))
    (cond
      ((= i 1) xs)
      (else (heap-sort-i (swap xs 0 (- i 1)) (- i 1))))))
  (heap-sort-i (build-max-heap xs)))
```

Este código contiene funciones para realizar diversas operaciones matemáticas y de procesamiento de listas. Las funciones son:

* `factorial`: Calcula el factorial de un número.
* `fibonacci`: Calcula el número de Fibonacci de un índice dado.
* `es-primo?`: Comprueba si un número es primo.
* `es-palindromo?`: Comprueba si una lista es un palíndromo.
* `merge`: Fusiona dos listas ordenadas en una sola lista ordenada.
* `quicksort`: Ordena una lista usando el algoritmo de ordenación rápida.
* `heap-sort`: Ordena una lista usando el algoritmo de ordenación por montículos.

Estas funciones se pueden utilizar para resolver una amplia variedad de problemas. Por ejemplo, la función `factorial` se puede utilizar para calcular el número de permutaciones de un conjunto de elementos, la función `fibonacci` se puede utilizar para generar números de Fibonacci, la función `es-primo?` se puede utilizar para encontrar números primos, la función `es-palindromo?` se puede utilizar para comprobar si una palabra es un palíndromo, la función `merge` se puede utilizar para fusionar dos listas ordenadas, la función `quicksort` se puede utilizar para ordenar una lista y la función `heap-sort` se puede utilizar para ordenar una lista.