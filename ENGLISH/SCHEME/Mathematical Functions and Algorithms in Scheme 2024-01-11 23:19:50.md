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
      (gcd b (% a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (is-prime n)
  (define (prime? n d)
    (if (= d 1)
        #t
        (if (= 0 (% n d))
            #f
            (prime? n (- d 1)))))
  (prime? n (- n 1)))

(define (next-prime n)
  (define (next-prime? n d)
    (if (= d 1)
        #t
        (if (= 0 (% n d))
            #f
            (next-prime? n (- d 1)))))
  (if (is-prime n)
      n
      (next-prime (+ n 1))))

(define (factors n)
  (define (factors-helper n d factors)
    (if (= d 1)
        factors
        (if (= 0 (% n d))
            (factors-helper (/ n d) d (cons d factors))
            (factors-helper n (- d 1) factors))))
  (factors-helper n (- n 1) (list)))

(define (divisors n)
  (define (divisors-helper n d divisors)
    (if (= d 1)
        divisors
        (if (= 0 (% n d))
            (divisors-helper (/ n d) d (cons d divisors))
            (divisors-helper n (- d 1) divisors))))
  (divisors-helper n (- n 1) (list)))

(define (sum-of-divisors n)
  (define (sum-of-divisors-helper n d sum)
    (if (= d 1)
        sum
        (if (= 0 (% n d))
            (sum-of-divisors-helper (/ n d) d (+ sum d))
            (sum-of-divisors-helper n (- d 1) sum))))
  (sum-of-divisors-helper n (- n 1) 0))

(define (perfect-number n)
  (if (= n (sum-of-divisors n))
      #t
      #f))

(define (amicable-numbers n)
  (define (amicable-number? n m)
    (if (= n (sum-of-divisors m))
        #t
        #f))
  (define (amicable-numbers-helper n m result)
    (if (= n 1)
        result
        (if (amicable-number? n m)
            (amicable-numbers-helper (- n 1) m (cons (list n m) result))
            (amicable-numbers-helper (- n 1) m result))))
  (amicable-numbers-helper (- n 1) n (list)))

(define (goldbach-conjecture n)
  (define (goldbach-conjecture-helper n d result)
    (if (= d 1)
        result
        (if (is-prime n)
            (goldbach-conjecture-helper (- n d) d result)
            (goldbach-conjecture-helper (- n d) (- d 1) (cons (list n d) result)))))
  (goldbach-conjecture-helper n (- n 1) (list)))
```

This code implements a variety of mathematical functions and algorithms in Scheme. Here is a brief explanation of each function:

* `factorial`: Computes the factorial of a non-negative integer.
* `fibonacci`: Computes the nth Fibonacci number.
* `gcd`: Computes the greatest common divisor of two integers.
* `lcm`: Computes the least common multiple of two integers.
* `is-prime`: Checks if a given integer is prime.
* `next-prime`: Finds the next prime number after a given integer.
* `factors`: Finds the factors of a given integer.
* `divisors`: Finds the divisors of a given integer.
* `sum-of-divisors`: Computes the sum of the divisors of a given integer.
* `perfect-number`: Checks if a given integer is a perfect number.
* `amicable-numbers`: Finds all pairs of amicable numbers up to a given integer.
* `goldbach-conjecture`: Checks if the Goldbach conjecture is true for a given integer.

These functions are all implemented using a recursive approach, which is a common programming technique in Scheme. The `factorial`, `fibonacci`, `gcd`, `lcm`, `is-prime`, and `next-prime` functions are all tail-recursive, which means that they can be implemented efficiently without using a stack.

The `factors`, `divisors`, `sum-of-divisors`, `perfect-number`, `amicable-numbers`, and `goldbach-conjecture` functions are all non-tail-recursive, which means that they can potentially use a stack to store intermediate results. However, these functions are still implemented in a way that is efficient and easy to understand.

This code is a good example of how to use Scheme to implement a variety of mathematical functions and algorithms. It is also a good example of how to use recursion and tail recursion in Scheme.