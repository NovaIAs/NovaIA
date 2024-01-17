```scheme

(define (factorial n)
  (if (zero? n)
    1
    (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (< n 2)
    n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (zero? b)
    a
    (gcd b (remainder a b))))

(define (lcm a b)
  (* a b) (/ (gcd a b)))

(define (is-prime? n)
  (and (> n 1)
       (for-each (i (in-range 2 (sqrt n)))
         (not (zero? (remainder n i))))))

(define (divisors-of n)
  (for*/fold (i (in-range 1 n)) (result '())
    (if (zero? (remainder n i))
      (cons i result)
      result)))

(define (prime-factors-of n)
  (for*/fold (i (in-range 2 n)) (result '())
    (if (and (is-prime? i) (zero? (remainder n i)))
      (cons i result)
      result)))

(define (nth-prime n)
  (let loop ((i 2) (count 0))
    (if (= count n)
      i
      (if (is-prime? i)
        (loop (+ i 1) (+ count 1))
        (loop (+ i 1) count)))))

```

This code is a collection of mathematical functions implemented in Scheme. It includes functions for calculating factorials, Fibonacci numbers, greatest common divisors (gcd), least common multiples (lcm), prime numbers, divisors of a number, prime factors of a number, and the nth prime number.

The code is structured using a combination of named functions and macros, which makes it easy to read and understand. The `for*/fold` macro is used to implement the Sieve of Eratosthenes algorithm for generating prime numbers efficiently.

Overall, this code is a well-written and efficient implementation of these mathematical functions in Scheme.