```scheme

(define (factorial n)
  (if (= n 0)
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

(define (is-prime n)
  (and (> n 1)
       (every? (lambda (d) (= (remainder n d) 0))
               (range 2 (sqrt n)))))

(define (next-prime n)
  (if (is-prime (+ n 1))
      (+ n 1)
      (next-prime (+ n 1))))

(define (prime-factors n)
  (if (is-prime n)
      '(n)
      (let ((p (next-prime 1)))
        (if (= (remainder n p) 0)
            (cons p (prime-factors (/ n p)))
            (prime-factors n)))))

(define (is-perfect-number n)
  (= n (sum (filter (lambda (d) (is-prime d)) (divisors n)))))

(define (amicable-pair a b)
  (and (= a (sum (divisors b))) (= b (sum (divisors a)))))

(define (goldbach-conjecture n)
  (every? (lambda (p) (is-prime p)) (prime-factors n))
        (every? (lambda (p) (is-perfect-number (+ p (/ n p)))) (prime-factors n))))

(define (mergesort lst)
  (if (null? lst)
      '()
      (let ((mid (quotient (length lst) 2)))
        (append (mergesort (take lst mid)) (mergesort (drop lst mid))))))

(define (quicksort lst)
  (if (null? lst)
      '()
      (let ((pivot (car lst))
            (left (filter (lambda (x) (< x pivot)) (cdr lst)))
            (right (filter (lambda (x) (>= x pivot)) (cdr lst))))
        (append (quicksort left) pivot (quicksort right))))))

(define (radix-sort lst)
  (if (null? lst)
      '()
      (let ((max (apply max lst)))
        (for/list ((exp 1)
                   #:while (< exp max)
                   #:do (set! exp (* exp 10)))))
        (for*/fold ((sorted '())
                   #:init sorted)
                  ((digit exp) #:from 0 #:to max #:by 10)
                  ((bucket '())
                   #:do (set-box! sorted 0
                                  (cons (filter (lambda (x) (= (quotient (/ x exp) 10) digit)) lst) (cdr sorted))))
                  sorted)))))

```

Este código es una colección de funciones matemáticas y de clasificación en SCHEME. Incluye funciones para calcular el factorial, la secuencia de Fibonacci, el máximo común divisor (MCD), el mínimo común múltiplo (MCM), la primalidad, el siguiente primo, los factores primos, la perfección de un número, los pares amistosos, la conjetura de Goldbach, la ordenación por fusión, la ordenación rápida y la ordenación radix.

La función factorial calcula el factorial de un número. La función fibonacci calcula el n-ésimo número de Fibonacci. La función gcd calcula el MCD de dos números. La función lcm calcula el MCM de dos números. La función is-prime comprueba si un número es primo. La función next-prime devuelve el siguiente primo después de un número dado. La función prime-factors calcula los factores primos de un número. La función is-perfect-number comprueba si un número es perfecto. La función amicable-pair comprueba si dos números son pares amistosos. La función goldbach-conjecture comprueba si un número satisface la conjetura de Goldbach.

Las funciones mergesort, quicksort y radix-sort son algoritmos de ordenación. Mergesort utiliza la estrategia de dividir y conquistar para ordenar una lista. Quicksort utiliza la estrategia de seleccionar un pivote y dividir la lista en dos sublistas, una con elementos menores que el pivote y otra con elementos mayores o iguales que el pivote. Radix-sort utiliza la estrategia de ordenar los elementos de una lista según sus dígitos.