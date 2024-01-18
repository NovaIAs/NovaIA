```scheme
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (or (< n 2) (zero? n))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (/ (* a b) (gcd a b)))

(define (isPrime n)
  (if (or (<= n 1) (zero? n))
      #f
      (and (zero? (remainder n 2))
           (every? (lambda (d) (not (zero? (remainder n d))))
                   (range 3 (sqrt n) 2)))))

(define (combinations n r)
  (if (or (> r n) (= r 0))
      0
      (* (factorial n) (/ (factorial (- n r)) (factorial r)))))

(define (permutations n r)
  (if (or (> r n) (= r 0))
      0
      (* (factorial n) (/ (factorial (- n r)))))

(define (binomial n r)
  (/ (combinations n r) (factorial r))))
```

This code defines a collection of mathematical functions in Scheme:

* `factorial`: Calculates the factorial of a non-negative integer.
* `fibonacci`: Determines the nth Fibonacci number.
* `gcd`: Computes the greatest common divisor of two integers.
* `lcm`: Calculates the least common multiple of two integers.
* `isPrime`: Checks if a given integer is prime.
* `combinations`: Determines the number of ways to choose a subset of a given size from a set of elements.
* `permutations`: Calculates the number of ways to arrange a subset of a given size from a set of elements without regard to order.
* `binomial`: Determines the number of ways to choose a subset of a given size from a set of elements, where the order of the elements does not matter.