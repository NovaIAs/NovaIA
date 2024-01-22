```scheme
(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (or (= n 0) (= n 1))
    n
    (+ (fibonacci (- n 1))
       (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (prime-factors n)
  (define (prime-factors-helper n factors)
    (if (= n 1)
      factors
      (let loop ((n n) (factors factors))
        (if (zero? (remainder n 2))
          (loop (/ n 2) (cons 2 factors))
          (let ((factor (first (filter (lambda (factor) (= 0 (remainder n factor)))
                                    (range 3 (+ n 1) 2))))
            (if factor
              (loop (/ n factor) (cons factor factors))
              n))))))
  (prime-factors-helper n '()))

(define (nth-prime n)
  (define (is-prime n)
    (and (not (zero? n))
         (not (even? n))
         (null? (filter (lambda (factor) (= 0 (remainder n factor)))
                        (range 3 (+ n 1) 2)))))
  (define (nth-prime-helper n count)
    (if (= count n)
      n
      (let loop ((count count) (n (+ n 2)))
        (if (is-prime n)
          (nth-prime-helper n (+ count 1))
          (loop count (+ n 2))))))
  (nth-prime-helper n 1))

(define (quick-sort lst)
  (if (null? lst)
    '()
    (let ((pivot (car lst)))
      (append
        (quick-sort (filter (lambda (x) (< x pivot)) (cdr lst)))
        (list pivot)
        (quick-sort (filter (lambda (x) (>= x pivot)) (cdr lst)))))))

(define (merge-sort lst)
  (if (null? lst)
    '()
    (let loop ((lst lst) (sorted '()))
      (if (null? lst)
        sorted
        (let ((mid (quotient (length lst) 2)))
          (loop (cdr lst)
                (append
                  sorted
                  (merge (car lst)
                         (merge-sort (take lst mid))
                         (merge-sort (drop lst (+ mid 1)))))))))))

(define (merge lst1 lst2)
  (define (merge-helper lst1 lst2 sorted)
    (if (null? lst1)
      (append sorted lst2)
      (if (null? lst2)
        (append sorted lst1)
        (let ((elem1 (car lst1)))
          (let ((elem2 (car lst2)))
            (if (< elem1 elem2)
              (merge-helper (cdr lst1) lst2 (cons elem1 sorted))
              (merge-helper lst1 (cdr lst2) (cons elem2 sorted))))))))
  (merge-helper lst1 lst2 '()))

(define (binary-search lst target)
  (define (binary-search-helper lst target low high)
    (if (> high low)
      (let ((mid (quotient (+ low high) 2)))
        (if (= (car (elt lst mid)) target)
          mid
          (if (< (car (elt lst mid)) target)
            (binary-search-helper lst target (+ mid 1) high)
            (binary-search-helper lst target low (- mid 1)))))
      -1))
  (binary-search-helper lst target 0 (- (length lst) 1)))

(define (dijkstra-shortest-path graph start end)
  (define (dijkstra-helper graph start end visited distances)
    (if (= start end)
      distances
      (let ((neighbors (assoc start graph)))
        (if (null? neighbors)
          distances
          (let loop ((neighbors neighbors) (visited visited) (distances distances))
            (if (null? neighbors)
              distances
              (let ((neighbor (car neighbors)))
                (let ((new-distance (+ (car distances) (caar neighbor))))
                  (if (or (null? (assoc neighbor distances))
                          (> (car (assoc neighbor distances)) new-distance))
                    (loop (cdr neighbors)
                          (cons neighbor visited)
                          (assoc neighbor distances new-distance))
                    (loop (cdr neighbors) visited distances)))))))))
  (dijkstra-helper graph start end '() (list (cons start 0))))

```

Explicación del código:

* **Factorial:** Calcula el factorial de un número.
* **Fibonacci:** Calcula el n-ésimo número de Fibonacci.
* **Gcd:** Calcula el máximo común divisor de dos números.
* **Lcm:** Calcula el mínimo común múltiplo de dos números.
* **Prime-factors:** Calcula los factores primos de un número.
* **Nth-prime:** Calcula el n-ésimo número primo.
* **Quick-sort:** Implementación del algoritmo de ordenamiento rápido.
* **Merge-sort:** Implementación del algoritmo de ordenamiento por fusión.
* **Merge:** Función auxiliar para el algoritmo de ordenamiento por fusión.
* **Binary-search:** Implementación del algoritmo de búsqueda binaria.
* **Dijkstra-shortest-path:** Implementación del algoritmo de Dijkstra para encontrar el camino más corto en un grafo.