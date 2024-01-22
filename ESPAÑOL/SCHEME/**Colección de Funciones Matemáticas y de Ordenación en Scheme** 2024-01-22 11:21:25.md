```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (exponentiation a n)
  (if (= n 0)
      1
      (* a (exponentiation a (- n 1)))))

(define (is-prime? n)
  (if (<= n 1)
      #f
      (and (>= n 2)
           (for/or ([i (in-range 2 (sqrt n))])
               (= (remainder n i) 0)))))

(define (nth-prime n)
  (define (is-prime? n)
    (if (<= n 1)
        #f
        (and (>= n 2)
             (for/or ([i (in-range 2 (sqrt n))])
                 (= (remainder n i) 0)))))
  (define prime-counter 0)
  (define current-number 2)
  (while (< prime-counter n)
    (if (is-prime? current-number)
        (set! prime-counter (+ prime-counter 1))
        (set! current-number (+ current-number 1)))
    (set! current-number (+ current-number 1)))
  (- current-number 1))

(define (merge a b)
  (if (empty? a)
      b
      (if (empty? b)
          a
          (cons (car a) (merge (cdr a) b)))))

(define (merge-sort lst)
  (if (empty? lst)
      '()
      (let ([pivot (car lst)]
            [rest (cdr lst)])
        (merge (merge-sort (filter (lambda (x) (< x pivot)) rest))
               (cons pivot (merge-sort (filter (lambda (x) (>= x pivot)) rest)))))))

(define (quick-sort lst)
  (if (empty? lst)
      '()
      (let ([pivot (car lst)]
            [rest (cdr lst)])
        (append
         (quick-sort (filter (lambda (x) (< x pivot)) rest))
         (cons pivot (quick-sort (filter (lambda (x) (>= x pivot)) rest)))))))

(define (binary-search lst x)
  (define (binary-search-aux lst x low high)
    (if (> low high)
        -1
        (let ([mid (/ (+ low high) 2)])
          (if (= (car lst mid) x)
              mid
              (if (< (car lst mid) x)
                  (binary-search-aux lst x (+ mid 1) high)
                  (binary-search-aux lst x low (- mid 1))))))))
  (binary-search-aux lst x 0 (- (length lst) 1)))
```

Explicación del código:

* La función `factorial` calcula el factorial de un número.
* La función `fibonacci` calcula el n-ésimo número de Fibonacci.
* La función `gcd` calcula el máximo común divisor de dos números.
* La función `exponentiation` calcula la potencia de un número.
* La función `is-prime?` comprueba si un número es primo.
* La función `nth-prime` calcula el n-ésimo número primo.
* La función `merge` fusiona dos listas ordenadas en una sola lista ordenada.
* La función `merge-sort` ordena una lista usando el algoritmo de ordenación por fusión.
* La función `quick-sort` ordena una lista usando el algoritmo de ordenación rápida.
* La función `binary-search` busca un elemento en una lista ordenada usando el algoritmo de búsqueda binaria.