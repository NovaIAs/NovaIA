```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (is-prime n)
  (if (<= n 1)
      #f
      (let loop ((divisor 2)
                (is-prime? #t))
        (if (> divisor (sqrt n))
            is-prime?
            (if (= (remainder n divisor) 0)
                #f
                (loop (+ divisor 1) is-prime?))))))

(define (sieve n)
  (let loop ((i 2)
            (primes '()))
    (if (> i n)
        primes
        (let ((is-prime? (is-prime i)))
          (if is-prime?
              (loop (+ i 1) (cons i primes))
              (loop (+ i 1) primes))))))

(define (merge xs ys)
  (cond
    [(null? xs)        ys]
    [(null? ys)        xs]
    [(< (car xs) (car ys)) (cons (car xs) (merge (cdr xs) ys))]
    [else (cons (car ys) (merge xs (cdr ys)))]))

(define (merge-sort xs)
  (cond
    [(null? xs)        '()]
    [(null? (cdr xs))  xs]
    [else (let ((p (length xs) 2))
              (merge (merge-sort (sublist xs 0 (- p 1)))
                     (merge-sort (sublist xs p (length xs)))))]))

(define (quicksort xs)
  (cond
    [(null? xs)        '()]
    [(= (length xs) 1) xs]
    [else (let ((p (car xs)))
              (merge (quicksort (filter (lambda (x) (< x p)) (cdr xs)))
                     (cons p)
                     (quicksort (filter (lambda (x) (>= x p)) (cdr xs)))))]))

(define (heapsort xs)
  (let loop ((xs xs)
            (i 0))
    (if (zero? i)
        (set-car! xs (apply max xs))
        (let ((l (left-child i))
              (r (right-child i))
              (max-child (max-child l r)))
          (if (< (car xs) (car (list-ref xs max-child)))
              (let ((temp (car xs)))
                (set-car! xs (car (list-ref xs max-child)))
                (set-car! (list-ref xs max-child) temp))
              (set-car! xs (car (list-ref xs i))))
          (loop (list-ref xs (parent i)) (sub1 i))))))

(define (left-child i)
  (+ (* i 2) 1))

(define (right-child i)
  (+ (* i 2) 2))

(define (parent i)
  (quotient (- i 1) 2))

(define (max-child l r)
  (if (>= l (length xs))
      r
      (if (>= r (length xs))
          l
          (if (> (car (list-ref xs l)) (car (list-ref xs r)))
              l
              r))))

(define (insertion-sort xs)
  (let loop ((sorted '())
            (unsorted xs))
    (cond
      [(null? unsorted) sorted]
      [else (let ((v (car unsorted))
                (i (length sorted)))
              (loop (append (sublist sorted 0 i)
                            (cons v)
                            (sublist sorted i (length sorted)))
                     (cdr unsorted))))]))

(define (selection-sort xs)
  (let loop ((sorted '())
            (unsorted xs))
    (cond
      [(null? unsorted) sorted]
      [else (let ((min-value (apply min unsorted))
                (min-index (find min-value unsorted)))
              (set-car! (list-ref unsorted min-index)
                  (car (list-ref xs min-index)))
              (loop (cons min-value sorted)
                     (remove min-value unsorted))))]))

(define (bubble-sort xs)
  (cond
    [(or (null? xs) (= (length xs) 1)) xs]
    [else (let ((x (car xs))
                (xs (cdr xs)))
              (append (bubble-sort (remove x xs))
                     (cons x))))]))

(define (shake-sort xs)
  (let loop ((xs xs)
            (swapped? #t)
            (i 0)
            (j (- (length xs) 1)))
    (if (or (not swapped?)
            (zero? i)
            (>= j 0))
        xs
        (let ((x (car xs))
              (y (car (list-ref xs j))))
          (if (> y x)
              (set! xs (cons y (cdr xs)))
              (set! (list-ref xs j) x))
          (set! swapped? (> y x))
          (if (= i j)
              (set! i 0)
              (set! i (+ i 1)))
          (set! j (- j 1))
          (loop xs swapped? i j))))))
```

Este código contiene una colección de algoritmos de clasificación implementados en Scheme. Incluye algoritmos como el factorial, Fibonacci, máximo común divisor, mínimo común múltiplo, prueba de primalidad, criba de Eratóstenes, fusión, clasificación por fusión, clasificación rápida, clasificación por montículo, clasificación por inserción, clasificación por selección, clasificación por burbujas y clasificación por agitación. Cada algoritmo está implementado en su propia función y se explica brevemente su funcionamiento.