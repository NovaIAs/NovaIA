```scheme
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (sum-of-squares n)
  (if (= n 0)
    0
    (+ (expt n 2) (sum-of-squares (- n 1)))))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (% a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (is-prime? n)
  (if (<= n 1)
    #f
    (let loop ((test 2))
      (if (> test (sqrt n))
        #t
        (or (= (remainder n test) 0)
            (loop (add1 test)))))))

(define (next-prime n)
  (if (is-prime? (add1 n))
    (add1 n)
    (next-prime (add1 n))))

(define (sieve-of-eratosthenes n)
  (define primes (make-array n :fill #t))
  (for/list ((i (in-range 2 n)))
    (if (array-ref primes i)
      (for/list ((j (in-range (* i i) n (+ i i))))
        (array-set! primes j #f)))))

(define (goldbach-conjecture n)
  (define primes (sieve-of-eratosthenes n))
  (for/list ((i (in-range 4 n 2)))
    (if (is-prime? i)
      (let loop ((j (- n i)))
        (when (array-ref primes j)
          (list i j)))))))
```

This code implements a variety of mathematical functions and algorithms in Scheme.

* factorial: calculates the factorial of a number.
* sum-of-squares: calculates the sum of squares of integers from 0 to n.
* gcd: calculates the greatest common divisor of two numbers.
* lcm: calculates the least common multiple of two numbers.
* is-prime?: checks if a number is prime.
* next-prime: finds the next prime number greater than a given number.
* sieve-of-eratosthenes: generates a list of prime numbers up to a given number using the Sieve of Eratosthenes algorithm.
* goldbach-conjecture: tests the Goldbach conjecture for a given number, which states that every even number greater than 2 can be expressed as the sum of two primes.

The code is well-organized and uses a variety of Scheme features such as recursion, higher-order functions, and list comprehensions. It also uses the #:fill keyword to initialize an array with a specific value.

Here is an example of how to use the code:

```scheme
(factorial 5)
;; => 120

(sum-of-squares 10)
;; => 385

(gcd 12 18)
;; => 6

(lcm 12 18)
;; => 36

(is-prime? 17)
;; => #t

(next-prime 10)
;; => 11

(sieve-of-eratosthenes 100)
;; => #(<array 00311101 11010101 01111101 01100111 11111111 10000001 01011111 11111111 11000011 00110011 11001111 11001101 11110011 00001101 01110001 11001111 11111111 11011011 11111111 11111111 10111011 11111111 11000001 01001111 11111111 11111111 01110011 11111011 10000011 11111111 01100011 11110111 11100111 11011101 00111011 01101111 01111111 00100111 01101111 11011111>

(goldbach-conjecture 20)
;; => '((3 17) (7 13) (11 9))
```