```scheme
(define (fac n)
  (cond ((zero? n) 1) (else (* n (fac (- n 1))))))

(define (fib n)
  (cond ((zero? n) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (gcd a b)
  (cond ((zero? b) a) (else (gcd b (remainder a b)))))

(define (lcm a b)
  (/ (* a b) (gcd a b)))

(define (prime? n)
  (cond ((= n 2) #t) ((zero? (remainder n 2)) #f) (else (let loop ((d 3))
    (cond ((>= d (sqrt n)) #t) ((zero? (remainder n d)) #f) (else (loop (+ d 2)))))))))

(define (factors n)
  (cond ((prime? n) (list n)) (else (let loop ((d 2) (res '()))
    (cond ((>= d (sqrt n)) res) ((zero? (remainder n d)) (loop (+ d 1) (cons d res))) (else (loop (+ d 1) res)))))))

(define (perfect? n)
  (= n (apply + (factors n))))

(define (amicable? a b)
  (and (= a (apply + (factors b))) (= b (apply + (factors a))))))

(define (goldbach? n)
  (let loop ((p 3))
    (cond ((> p (sqrt n)) #f) ((prime? p) (let ((q (- n p))) (and (prime? q) (= n (+ p q))))) (else (loop (+ p 2)))))))

(define (catalan n)
  (if (zero? n) 1 (let loop ((res 0) (i 1))
    (cond ((= i n) res) (else (loop (+ res (* i (- n i))) (+ i 1)))))))

(define (lucas n)
  (cond ((zero? n) 2) ((= n 1) 1) (else (+ (lucas (- n 1)) (lucas (- n 2))))))

(define (tribonacci n)
  (cond ((zero? n) 0) ((= n 1) 0) ((= n 2) 1) (else (+ (tribonacci (- n 1)) (tribonacci (- n 2)) (tribonacci (- n 3))))))

(define (fibonacci-sequence n)
  (for/list ((i (in-range 0 n))) (fib i)))

(define (lucas-sequence n)
  (for/list ((i (in-range 0 n))) (lucas i)))

(define (tribonacci-sequence n)
  (for/list ((i (in-range 0 n))) (tribonacci i)))

(define (perfect-numbers n)
  (for/list ((i (in-range 1 n)) #f) (when (perfect? i) i)))

(define (amicable-pairs n)
  (for/list ((a (in-range 1 n)) (b (in-range 1 n)) #f) (when (amicable? a b) (list a b))))

(define (goldbach-conjectures n)
  (for/list ((i (in-range 4 n)) #f) (when (goldbach? i) i)))
```

This code implements a variety of mathematical functions and sequences. Here's a brief explanation of each function:

```scheme
(fac n)
```

Calculates the factorial of a non-negative integer n.

```scheme
(fib n)
```

Calculates the nth Fibonacci number.

```scheme
(gcd a b)
```

Calculates the greatest common divisor of two integers a and b.

```scheme
(lcm a b)
```

Calculates the least common multiple of two integers a and b.

```scheme
(prime? n)
```

Checks if a given integer n is prime.

```scheme
(factors n)
```

Returns a list of the prime factors of an integer n.

```scheme
(perfect? n)
```

Checks if an integer n is perfect, meaning that the sum of its proper divisors is equal to n itself.

```scheme
(amicable? a b)
```

Checks if two integers a and b are amicable numbers, meaning that the sum of the proper divisors of a is equal to b, and vice versa.

```scheme
(goldbach? n)
```

Checks if a given even integer n greater than 2 can be expressed as the sum of two primes.

```scheme
(catalan n)
```

Calculates the nth Catalan number.

```scheme
(lucas n)
```

Calculates the nth Lucas number.

```scheme
(tribonacci n)
```

Calculates the nth Tribonacci number.

```scheme
(fibonacci-sequence n)
```

Generates a list of the first n Fibonacci numbers.

```scheme
(lucas-sequence n)
```

Generates a list of the first n Lucas numbers.

```scheme
(tribonacci-sequence n)
```

Generates a list of the first n Tribonacci numbers.

```scheme
(perfect-numbers n)
```

Generates a list of all perfect numbers up to n.

```scheme
(amicable-pairs n)
```

Generates a list of all amicable pairs of numbers up to n.

```scheme
(goldbach-conjectures n)
```

Generates a list of all even integers up to n that can be expressed as the sum of two primes.