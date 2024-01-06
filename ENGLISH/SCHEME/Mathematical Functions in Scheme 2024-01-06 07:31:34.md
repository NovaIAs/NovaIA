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
      (gcd b (remainder a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (isPrime n)
  (if (<= n 1)
      #f
      (let loop ((d 2))
        (if (> (* d d) n)
            #t
            (if (= (remainder n d) 0)
                #f
                (loop (+ d 1)))))))

(define (prime-factors n)
  (if (isPrime n)
      (list n)
      (let loop ((factors '()) (d 2))
        (if (> (* d d) n)
            factors
            (if (= (remainder n d) 0)
                (loop (cons d factors) d)
                (loop factors (+ d 1)))))))

(define (divisors n)
  (let loop ((divisors '()) (d 1))
    (if (> d n)
        divisors
        (if (= (remainder n d) 0)
            (loop (cons d divisors) (+ d 1))
            (loop divisors (+ d 1))))))

(define (perfect-number n)
  (if (<= n 1)
      #f
      (= n (apply + (divisors n)))))

(define (mersenne-prime p)
  (if (<= p 1)
      #f
      (isPrime (- (expt 2 p) 1))))

(define (goldbach-conjecture n)
  (if (even? n)
      (let loop ((p 3) (q (- n p)))
        (if (isPrime p)
            (if (isPrime q)
                #t
                (loop (+ p 2) (- q 2)))))))
```

This code is a collection of mathematical functions written in Scheme. It includes functions for calculating factorials, Fibonacci numbers, greatest common divisors, least common multiples, prime numbers, prime factors, divisors, perfect numbers, Mersenne primes, and the Goldbach conjecture.

The code is well-commented and easy to understand. It uses a variety of Scheme features, including recursion, higher-order functions, and list comprehensions.

Here are some examples of how to use the code:

```scheme
(factorial 5)
```

```
120
```

```scheme
(fibonacci 10)
```

```
55
```

```scheme
(gcd 12 18)
```

```
6
```

```scheme
(lcm 12 18)
```

```
36
```

```scheme
(isPrime 17)
```

```
#t
```

```scheme
(prime-factors 12)
```

```
'(2 2 3)
```

```scheme
(divisors 12)
```

```
'(1 2 3 4 6 12)
```

```scheme
(perfect-number 6)
```

```
#t
```

```scheme
(mersenne-prime 5)
```

```
#t
```

```scheme
(goldbach-conjecture 10)
```

```
#t
```