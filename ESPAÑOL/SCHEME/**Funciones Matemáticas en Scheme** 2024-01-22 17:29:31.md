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
    (gcd b (% a b))))

(define (lcm a b)
  (/ (* a b) (gcd a b)))

(define (isPrime n)
  (if (<= n 1)
    #f
    (or (not (even? n))
        (every? (lambda (x) (not (= 0 (% n x))))
               (iota 3 n 2)))))

(define (greatest-common-divisor a b)
  (if (= b 0)
    a
    (greatest-common-divisor b (% a b))))

(define (least-common-multiple a b)
  (/ (* a b) (greatest-common-divisor a b))))

```

Explicación del código:

* La función `factorial` calcula el factorial de un número.
* La función `fibonacci` calcula el número de Fibonacci en la posición n.
* La función `gcd` calcula el máximo común divisor de dos números.
* La función `lcm` calcula el mínimo común múltiplo de dos números.
* La función `isPrime` comprueba si un número es primo.
* La función `greatest-common-divisor` calcula el máximo común divisor de dos números.
* La función `least-common-multiple` calcula el mínimo común múltiplo de dos números.