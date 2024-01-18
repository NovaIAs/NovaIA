```scheme
;; Define a function to calculate the factorial of a number
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; Define a function to calculate the sum of the digits of a number
(define (sum-of-digits n)
  (if (< n 10)
      n
      (+ (mod n 10) (sum-of-digits (/ n 10)))))

;; Define a function to check if a number is prime
(define (is-prime n)
  (define (helper n d)
    (or (= d n)
        (and (< (* d d) n) (helper n (+ d 1)))))
  (helper n 2))

;; Define a function to find the greatest common divisor of two numbers
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Define a function to find the least common multiple of two numbers
(define (lcm a b)
  (* a b) (/ (gcd a b)))

;; Define a function to generate a list of prime numbers up to a given number
(define (primes-up-to n)
  (define (helper n primes)
    (if (> n 1)
        (if (is-prime n)
            (helper (- n 1) (cons n primes))
            (helper (- n 1) primes))
        primes))
  (helper n '()))

;; Define a function to calculate the Fibonacci sequence up to a given number
(define (fibonacci n)
  (define (helper n a b)
    (if (= n 0)
        a
        (helper (- n 1) b (+ a b))))
  (helper n 0 1))

;; Define a function to calculate the binomial coefficient of two numbers
(define (binomial-coefficient n k)
  (if (or (< n k) (< k 0))
      0
      (/ (factorial n) (* (factorial (- n k)) (factorial k)))))
```

This code is a collection of various mathematical functions implemented in Scheme. Here's a brief explanation of each function:

```scheme
;; factorial: Calculates the factorial of a given number n.

;; sum-of-digits: Calculates the sum of the digits of a given number n.

;; is-prime: Checks if a given number n is prime.

;; gcd: Calculates the greatest common divisor of two given numbers a and b.

;; lcm: Calculates the least common multiple of two given numbers a and b.

;; primes-up-to: Generates a list of prime numbers up to a given number n.

;; fibonacci: Calculates the nth number in the Fibonacci sequence.

;; binomial-coefficient: Calculates the binomial coefficient of two given numbers n and k.
```

These functions cover a wide range of mathematical concepts and can be used for various purposes in programming.