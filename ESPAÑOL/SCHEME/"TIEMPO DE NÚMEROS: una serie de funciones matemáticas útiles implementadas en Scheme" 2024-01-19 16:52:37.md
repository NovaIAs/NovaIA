```scheme
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (fibonacci (- n 1))
             (fibonacci (- n 2))))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (coprime? a b)
  (= 1 (gcd a b)))

(define (next-prime n)
  (define is-prime (lambda (i) (coprime? n i)))
  (define try (lambda (i) (if (is-prime i) (i) (try (+ i 1)))))
  (try (+ n 1)))

(define (sieve n)
  (define is-prime (lambda (i) (coprime? n i)))
  (define sieve-helper (lambda (i primes)
    (if (is-prime i)
        (cons i (sieve-helper (next-prime i) primes))
        (sieve-helper (next-prime i) primes))))
  (sieve-helper 2 '()))

(define (primes-up-to n)
  (sieve n))

(define (goldbach-conjecture n)
  (define primes (primes-up-to n))
  (define is-goldbach (lambda (i)
    (define half (- i 2)
          found (filter (lambda (p) (member (- half p) primes))
                        primes)))
  (filter is-goldbach (range 4 (+ n 1))))

(define (sum-of-digits n)
  (if (= n 0)
      0
      (+ (remainder n 10) (sum-of-digits (quotient n 10)))))

(define (armstrong? n)
  (= n (sum-of-digits (expt n (length (number->string n))))))

(define (happy? n)
  (define is-happy (lambda (i)
    (if (= i 1)
        true
        (is-happy (sum-of-digits (expt i 2))))))
  (is-happy n))

(define (perfect-number? n)
  (define divisors (filter
                      (lambda (i) (= 0 (remainder n i)))
                      (range 1 n)))
  (= n (sum divisors)))

(define (abundant? n)
  (> (sum (filter (lambda (i) (= 0 (remainder n i)))
                 (range 1 n)))
     n))

(define (deficient? n)
  (< (sum (filter (lambda (i) (= 0 (remainder n i)))
                 (range 1 n)))
     n))
```

Este código implementa una variedad de funciones matemáticas en SCHEME. Las funciones incluyen:

* **factorial**: calcula el factorial de un número entero no negativo.
* **fibonacci**: calcula el n-ésimo número de Fibonacci.
* **gcd**: calcula el máximo común divisor de dos números enteros positivos.
* **lcm**: calcula el mínimo común múltiplo de dos números enteros positivos.
* **coprime?**: devuelve true si dos números enteros positivos son primos entre sí, y false en caso contrario.
* **next-prime**: calcula el siguiente primo después de un número entero positivo dado.
* **sieve**: genera una lista de todos los números primos hasta un número entero positivo dado.
* **primes-up-to**: devuelve una lista de todos los números primos hasta un número entero positivo dado.
* **goldbach-conjecture**: devuelve una lista de todos los enteros pares hasta un número entero positivo dado que se pueden expresar como la suma de dos primos.
* **sum-of-digits**: calcula la suma de los dígitos de un número entero positivo.
* **armstrong?**: devuelve true si un número entero positivo es un número de Armstrong, y false en caso contrario.
* **happy?**: devuelve true si un número entero positivo es un número feliz, y false en caso contrario.
* **perfect-number?**: devuelve true si un número entero positivo es un número perfecto, y false en caso contrario.
* **abundant?**: devuelve true si un número entero positivo es un número abundante, y false en caso contrario.
* **deficient?**: devuelve true si un número entero positivo es un número deficiente, y false en caso contrario.