```scheme
(define (sum-of-squares n)
  (if (= n 0)
    0
    (+ (square n) (sum-of-squares (- n 1)))))

(define (square n)
  (* n n))

(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (lcm a b)
  (* a b) (gcd a b))

(define (is-prime? n)
  (if (= n 2)
    #t
    (and (odd? n)
         (for/all? (in-range 3 (sqrt n) 2)
                   (not (= (remainder n i) 0))))))

(define (next-prime n)
  (if (is-prime? (1+ n))
    (+ 1 n)
    (next-prime (+ 1 n))))

(define (sqrt n)
  (sqrt-iter 1.0 n))

(define (sqrt-iter guess n)
  (if (< (abs (- guess (* guess guess) n)) 0.0001)
    guess
    (sqrt-iter (/ (+ guess (/ n guess)) 2.0) n)))

(define (linear-search xs x)
  (if (null? xs)
    #f
    (if (= (car xs) x)
      #t
      (linear-search (cdr xs) x))))

(define (binary-search xs x)
  (define (search lo hi)
    (define mid (floor (/ (+ lo hi) 2.0)))
    (if (= (car xs mid) x)
      mid
      (if (< x (car xs mid))
        (search lo (- mid 1))
        (search (+ mid 1) hi)))))

  (if (null? xs)
    #f
    (search 0 (- (length xs) 1))))
```

This Scheme code defines a variety of mathematical and algorithmic functions. Here's an explanation of each function:

1. `(sum-of-squares n)`: Calculates the sum of squares of integers from 1 to `n`.

2. `(square n)`: Squares a number `n`.

3. `(factorial n)`: Calculates the factorial of `n`.

4. `(gcd a b)`: Finds the greatest common divisor of two numbers `a` and `b`.

5. `(lcm a b)`: Calculates the least common multiple of two numbers `a` and `b`.

6. `(is-prime? n)`: Checks if a given number `n` is prime.

7. `(next-prime n)`: Finds the next prime number greater than `n`.

8. `(sqrt n)`: Calculates the square root of a number `n` using an iterative method.

9. `(linear-search xs x)`: Performs a linear search for an element `x` in a list `xs`.

10. `(binary-search xs x)`: Implements a binary search algorithm to find an element `x` in a sorted list `xs`.

These functions demonstrate different mathematical and algorithmic concepts, making this code complex and versatile. It covers various topics, including number theory, mathematical functions, and search algorithms.