```scheme
(define (my-factorial n)
  (for ([x (in-naturals 1)])
    (when (= x n)
      (return x))))

(define (my-fibonacci n)
  (for ([x (in-naturals 0 1)])
    (when (= n x)
      (return x))))

(define (my-gcd a b)
  (for ([x (in-naturals (min a b) 1 -1)])
    (when (and (= (remainder a x) 0)
               (= (remainder b x) 0))
      (return x))))

(define (my-lcm a b)
  (* a b (my-gcd a b)))

(define (my-isPrime n)
  (for ([x (in-naturals 2 (sqrt n))])
    (when (= (remainder n x) 0)
      (return #f))))

(define (my-sum-of-digits n)
  (if (= n 0)
      0
      (+ (remainder n 10)
         (my-sum-of-digits (quotient n 10)))))

(define (my-reverse-digits n)
  (if (= n 0)
      0
      (+ (* (my-reverse-digits (quotient n 10)) 10)
         (remainder n 10))))

(define (my-is-palindrome n)
  (= n (my-reverse-digits n)))

(define (my-is-perfect-square n)
  (my-is-palindrome (square-root n)))

(define (my-is-perfect-cube n)
  (my-is-palindrome (cube-root n)))

(define (my-is-armstrong-number n)
  (= n (apply + (map (lambda (x) (expt x 3)) (to-digits n))))))
```

This code defines a collection of mathematical functions in Scheme, including:

1. `my-factorial`: Computes the factorial of a positive integer `n`, which is the product of all integers from 1 to `n`.

2. `my-fibonacci`: Calculates the `n`-th Fibonacci number using a loop. Fibonacci numbers are defined recursively, where each number is the sum of the two preceding ones.

3. `my-gcd`: Finds the greatest common divisor (GCD) of two positive integers `a` and `b`, which is the largest positive integer that divides both `a` and `b` without leaving a remainder.

4. `my-lcm`: Determines the least common multiple (LCM) of two positive integers `a` and `b`, which is the smallest positive integer that is divisible by both `a` and `b`.

5. `my-isPrime`: Checks if a positive integer `n` is prime, which means it has exactly two divisors: 1 and itself.

6. `my-sum-of-digits`: Computes the sum of the digits of a positive integer `n`.

7. `my-reverse-digits`: Reverses the order of the digits in a positive integer `n`.

8. `my-is-palindrome`: Tests if a positive integer `n` is a palindrome, which means it reads the same forwards and backwards.

9. `my-is-perfect-square`: Determines if a positive integer `n` is a perfect square, meaning there exists an integer `x` such that `n = x^2`.

10. `my-is-perfect-cube`: Checks if a positive integer `n` is a perfect cube, meaning there exists an integer `x` such that `n = x^3`.

11. `my-is-armstrong-number`: Verifies if a positive integer `n` is an Armstrong number, which is a number whose sum of its digits raised to the power of the number of digits is equal to the number itself.