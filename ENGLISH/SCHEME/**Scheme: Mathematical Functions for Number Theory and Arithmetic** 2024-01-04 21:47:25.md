```scheme
;; Define a function to calculate the factorial of a number
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; Define a function to check if a number is prime
(define (prime? n)
  (if (= n 1)
      #f
      (for/list ([i 2])
        (< i (sqrt n))
        (not (= 0 (modulo n i))))))

;; Define a function to find all the prime factors of a number
(define (prime-factors n)
  (if (prime? n)
      (list n)
      (let loop ([i 2] [factors '()])
        (if (>= i (sqrt n))
            factors
            (if (= 0 (modulo n i))
                (loop (+ i 1) (cons i factors))
                (loop (+ i 1) factors))))))

;; Define a function to calculate the greatest common divisor of two numbers
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

;; Define a function to calculate the least common multiple of two numbers
(define (lcm a b)
  (/ (* a b) (gcd a b))))

;; Define a function to find all the divisors of a number
(define (divisors n)
  (let loop ([i 1] [divisors '()])
    (if (> i (sqrt n))
        divisors
        (if (= 0 (modulo n i))
            (loop (+ i 1) (cons i (cons (/ n i) divisors)))
            (loop (+ i 1) divisors))))))

;; Define a function to find all the perfect numbers less than or equal to a given number
(define (perfect-numbers n)
  (let loop ([i 2] [perfect-numbers '()])
    (if (> i n)
        perfect-numbers
        (if (= i (reduce + (divisors i)))
            (loop (+ i 1) (cons i perfect-numbers))
            (loop (+ i 1) perfect-numbers))))))

;; Define a function to find all the amicable numbers less than or equal to a given number
(define (amicable-numbers n)
  (let loop ([i 2] [amicable-numbers '()])
    (if (> i n)
        amicable-numbers
        (let ([sum-of-divisors (reduce + (divisors i))])
          (if (= i (reduce + (divisors sum-of-divisors)))
              (loop (+ i 1) (cons (list i sum-of-divisors) amicable-numbers))
              (loop (+ i 1) amicable-numbers))))))
```

This code includes several complex and differentiated functions in Scheme, covering a wide range of mathematical concepts. Here's a brief explanation of each function:

1. **factorial**: Calculates the factorial of a given number.
2. **prime?**: Checks if a given number is prime.
3. **prime-factors**: Finds all the prime factors of a given number.
4. **gcd**: Calculates the greatest common divisor of two given numbers.
5. **lcm**: Calculates the least common multiple of two given numbers.
6. **divisors**: Finds all the divisors of a given number.
7. **perfect-numbers**: Finds all the perfect numbers less than or equal to a given number.
8. **amicable-numbers**: Finds all the amicable numbers less than or equal to a given number.

These functions demonstrate the versatility and power of Scheme for mathematical computations. They can be used for various purposes, such as number theory, cryptography, and algorithm design.