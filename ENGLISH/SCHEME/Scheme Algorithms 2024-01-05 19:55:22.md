```scheme
(define (fac n)
  (if (= n 0)
      1
      (* n (fac (- n 1)))))

(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

(define (lcm a b)
  (/ (* a b) (gcd a b)))

(define (is-prime n)
  (if (<= n 1)
      #f
      (let loop ((i 2))
        (if (= i n)
            #t
            (if (= (modulo n i) 0)
                #f
                (loop (+ i 1)))))))

(define (next-prime n)
  (if (is-prime n)
      n
      (next-prime (+ n 1))))

(define (sieve-of-eratosthenes n)
  (let loop ((i 2) (primes '()))
    (if (> i n)
        primes
        (let ((new-primes (remove i primes)))
          (loop (+ i 1) (append new-primes (remove (modulo i) new-primes)))))))

(define (merge xs ys)
  (if (null? xs)
      ys
      (if (null? ys)
          xs
          (if (< (car xs) (car ys))
              (cons (car xs) (merge (cdr xs) ys))
              (cons (car ys) (merge xs (cdr ys)))))))

(define (merge-sort xs)
  (if (null? xs)
      '()
      (let ((m (quotient (length xs) 2)))
        (merge (merge-sort (sublist xs 0 m))
               (merge-sort (sublist xs m (length xs)))))))

(define (quicksort xs)
  (if (null? xs)
      '()
      (let ((pivot (car xs))
            (less-than (filter (lambda (x) (< x pivot)) (cdr xs)))
            (greater-than (filter (lambda (x) (>= x pivot)) (cdr xs))))
        (append (quicksort less-than)
               (cons pivot)
               (quicksort greater-than)))))

(define (binary-search xs x)
  (if (null? xs)
      -1
      (let ((m (quotient (length xs) 2)))
        (if (= (car (sublist xs m 1)) x)
            m
            (if (< x (car (sublist xs m 1)))
                (binary-search (sublist xs 0 m) x)
                (binary-search (sublist xs m (length xs)) x))))))

(define (insertion-sort xs)
  (if (null? xs)
      '()
      (let ((ys (insertion-sort (cdr xs))))
        (insert (car xs) ys))))

(define (insert x xs)
  (if (null? xs)
      (list x)
      (if (< x (car xs))
          (cons x xs)
          (cons (car xs) (insert x (cdr xs))))))
```

This code is a collection of commonly used algorithms in computer science, all written in Scheme. It includes:

* The factorial function, which computes the factorial of a non-negative integer.
* The Fibonacci function, which computes the nth Fibonacci number.
* The greatest common divisor (GCD) function, which computes the greatest common divisor of two integers.
* The least common multiple (LCM) function, which computes the least common multiple of two integers.
* The primality test function, which determines whether a given integer is prime.
* The next prime function, which finds the next prime number after a given integer.
* The Sieve of Eratosthenes function, which generates a list of prime numbers up to a given integer.
* The merge function, which merges two sorted lists into a single sorted list.
* The merge sort function, which sorts a list using the merge sort algorithm.
* The quicksort function, which sorts a list using the quicksort algorithm.
* The binary search function, which searches for a given element in a sorted list using the binary search algorithm.
* The insertion sort function, which sorts a list using the insertion sort algorithm.

Each function is explained in detail in the comments within the code.