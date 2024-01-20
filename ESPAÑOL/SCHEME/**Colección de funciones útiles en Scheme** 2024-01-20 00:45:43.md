```scheme
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (prime? n)
  (if (= n 2)
      true
      (and (even? n) (prime? (/ n 2)))))

(define (nth-prime n)
  (let loop ((n n) (count 0) (p 2))
    (if (= count n)
        p
        (if (prime? p)
            (loop n (+ count 1) (+ p 1))
            (loop n count (+ p 1))))))

(define (merge xs ys)
  (cond
    [(null? xs) ys]
    [(null? ys) xs]
    [(< (car xs) (car ys)) (cons (car xs) (merge (cdr xs) ys))]
    [else (cons (car ys) (merge xs (cdr ys)))]))

(define (quicksort xs)
  (if (null? xs)
      '()
      (let ((pivot (car xs)) (less (filter (lambda (x) (< x pivot)) (cdr xs))) (greater (filter (lambda (x) (>= x pivot)) (cdr xs))))
        (append (quicksort less) (cons pivot) (quicksort greater)))))

(define (binary-search xs x)
  (define (loop left right)
    (if (= left right)
        (-1)
        (let ((mid (/ (+ left right) 2)))
          (if (= (car xs) x)
              mid
              (if (< (car xs) x)
                  (loop (+ mid 1) right)
                  (loop left (- mid 1)))))))
  (loop 0 (- (length xs) 1)))
```

Este código contiene una colección de funciones útiles en Scheme, incluyendo factoriales, Fibonacci, máximo común divisor, mínimo común múltiplo, comprobación de primalidad, cálculo del enésimo número primo, combinación de listas, clasificación rápida y búsqueda binaria. Cada función se explica brevemente en un comentario.