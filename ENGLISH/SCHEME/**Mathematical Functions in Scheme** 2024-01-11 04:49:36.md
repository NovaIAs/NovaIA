```scheme
(define (fact n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))

(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (% a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (is-prime n)
  (if (<= n 1)
      #f
      (or (>= n 2)
          (not (null? (filter (lambda (x) (= 0 (% n x)))
                              (range 2 (sqrt n)))))))

(define (prime-factors-list n)
  (define (prime-factors-list-helper n factors)
    (if (= n 1)
        factors
        (let loop ((n n) (factors factors))
          (if (is-prime n)
              (cons n factors)
              (let ((p (first (filter (lambda (x) (= 0 (% n x)))
                                     (range 2 (sqrt n))))))
                (loop (/ n p) (cons p factors)))))))
  (prime-factors-list-helper n '()))

(define (count-prime-factors n)
  (length (prime-factors-list n)))

(define (largest-prime-factor n)
  (last (prime-factors-list n)))

(define (sum-of-proper-divisors n)
  (let loop ((n n) (sum 0))
    (if (= n 1)
        sum
        (let ((p (first (filter (lambda (x) (= 0 (% n x)))
                                 (range 2 (sqrt n))))))
          (loop (/ n p) (+ sum (* p (+ 1 (sum-of-proper-divisors (/ n p)))))))))

(define (is-perfect-number n)
  (= n (sum-of-proper-divisors n)))

(define (mersenne-prime? n)
  (and (is-prime n) (= n (- (expt 2 n) 1))))

(define (mersenne-prime-list n)
  (filter mersenne-prime? (range 2 n)))

```

This code is a collection of mathematical functions implemented in Scheme. It includes functions for calculating factorials, Fibonacci numbers, greatest common divisors, least common multiples, testing for primality, finding prime factors, counting prime factors, finding the largest prime factor, summing the proper divisors of a number, testing for perfect numbers, testing for Mersenne primes, and generating a list of Mersenne primes.

Here's a brief explanation of each function:

1. `(fact n)`: Calculates the factorial of a non-negative integer `n`.

2. `(fib n)`: Calculates the `n`-th Fibonacci number.

3. `(gcd a b)`: Finds the greatest common divisor of two integers `a` and `b`.

4. `(lcm a b)`: Finds the least common multiple of two integers `a` and `b`.

5. `(is-prime n)`: Tests whether a given integer `n` is prime.

6. `(prime-factors-list n)`: Generates a list of the prime factors of a given integer `n`.

7. `(count-prime-factors n)`: Counts the number of prime factors of a given integer `n`.

8. `(largest-prime-factor n)`: Finds the largest prime factor of a given integer `n`.

9. `(sum-of-proper-divisors n)`: Calculates the sum of the proper divisors of a given integer `n`.

10. `(is-perfect-number n)`: Tests whether a given integer `n` is a perfect number, which means that it is equal to the sum of its proper divisors.

11. `(mersenne-prime? n)`: Tests whether a given integer `n` is a Mersenne prime, which is a prime number of the form `2^p - 1`, where `p` is prime.

12. `(mersenne-prime-list n)`: Generates a list of Mersenne primes up to a given integer `n`.

These functions cover a wide range of mathematical concepts and can be used for various purposes, such as number theory problems, cryptography, and algorithm analysis.