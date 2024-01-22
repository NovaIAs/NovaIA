```scheme
(define (factorial n)
  (if (= n 0) 1 (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (< n 2) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (greatest-common-divisor a b)
  (if (= b 0) a (greatest-common-divisor b (remainder a b))))

(define (least-common-multiple a b)
  (* a b (quotient (greatest-common-divisor a b) 1)))

(define (is-prime n)
  (cond
    [(<= n 1) false]
    [(= n 2) true]
    [else (and (for/list ([i (in-range 2 (- n 1))))
              (not (= (remainder n i) 0)))]))

(define (prime-factors n)
  (define (prime-factors-helper n factors)
    (cond
      [(is-prime n) (cons n factors)]
      [else (let ([i (min-prime-factor n)])
              (prime-factors-helper (quotient n i) (cons i factors)))]))
  (prime-factors-helper n '()))

(define (min-prime-factor n)
  (let loop ((i 2))
    (if (is-prime i)
      (if (= (remainder n i) 0) i (loop (+ i 1))))))

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

(define (lcm a b)
  (* a b (quotient (gcd a b) 1)))

(define (is-palindrome s)
  (let loop ((i 0) (j (- (string-length s) 1)))
    (if (or (>= i j) (= (string-ref s i) (string-ref s j)))
      true
      (loop (+ i 1) (- j 1))))))
```

Este código implementa una serie de funciones matemáticas comunes en Scheme. Las funciones incluyen:

* Factorial: calcula el factorial de un número.
* Fibonacci: calcula el número de Fibonacci de un número.
* Máximo común divisor: calcula el máximo común divisor de dos números.
* Mínimo común múltiplo: calcula el mínimo común múltiplo de dos números.
* Es primo: comprueba si un número es primo.
* Factores primos: calcula los factores primos de un número.
* Mínimo factor primo: calcula el factor primo más pequeño de un número.
* MCD: calcula el máximo común divisor de dos números.
* MCM: calcula el mínimo común múltiplo de dos números.
* Es palíndromo: comprueba si una cadena es un palíndromo.