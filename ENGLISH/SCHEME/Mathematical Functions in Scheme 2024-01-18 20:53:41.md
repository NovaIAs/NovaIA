```scheme
(define fibs '()) ; Initialize the Fibonacci sequence

(define (fib n)  ; Recursive Fibonacci function
  (if (< n 2) 1            ; Base case: return 1 for 0 and 1
      (+ (fib (- n 1))    ; Recursive case: add previous two numbers
         (fib (- n 2))) ))

(define (is-prime? n) ; Function to check if a number is prime
  (if (null? n) false  ; Base case: 0 and 1 are not prime
      (and (not (= n 1))  ; Base case: numbers less than 2 are not prime
           (every? (lambda (x) (not (= 0 (remainder n x)))) ; Recursive case
                  (range 2 (floor (sqrt n))))))))

(define (divisors n) ; Function to find all divisors of a number
  (filter (lambda (x) (zero? (remainder n x)))
          (range 1 (floor (+ n 1)))))

(define (perfect-number? n) ; Function to check if a number is perfect
  (and (= n (apply + (divisors (- n 1))))
       (is-prime? (apply + (divisors (- n 1))))))

(define (prime-factors n) ; Function to find all prime factors of a number
  (let loop ((n n) (factors '()))
    (if (and (is-prime? n) (not (zero? n)))
        (cons n factors)
        (let ((min-factor (min (divisors n))))
          (loop (/ n min-factor) (cons min-factor factors))))))

(define (greatest-common-divisor a b) ; Function to find the greatest common divisor
  (if (zero? b) a
      (greatest-common-divisor b (remainder a b))))

(define (least-common-multiple a b) ; Function to find the least common multiple
  (* (/ a (greatest-common-divisor a b)) b))

(display (prime? (fib 100))) ; Display if the 100th Fibonacci number is prime
```

Explanation:

1. We start by initializing the Fibonacci sequence as an empty list (`fibs` `'()`).

2. We define a recursive function `fib` to calculate Fibonacci numbers. It returns 1 for 0 and 1 (base cases). For other values of `n`, it adds the two preceding Fibonacci numbers.

3. We define a function `is-prime?` to check if a number is prime. It uses `every?` to check if the number is divisible by any number from 2 to its square root.

4. We define a function `divisors` to find all divisors of a number. It filters the range from 1 to `n+1` to keep only the numbers that divide `n` evenly.

5. We define a function `perfect-number?` to check if a number is perfect. A number is perfect if the sum of its proper divisors (divisors excluding the number itself) is equal to the number itself. It uses the `divisors` function to find the proper divisors.

6. We define a function `prime-factors` to find all prime factors of a number. It repeatedly divides the number by its smallest prime factor and adds that factor to the list of prime factors until the number becomes 1.

7. We define a function `greatest-common-divisor` to find the greatest common divisor of two numbers. It repeatedly divides the larger number by the smaller number until the remainder is 0.

8. We define a function `least-common-multiple` to find the least common multiple of two numbers. It multiplies the two numbers and divides the result by their greatest common divisor.

9. Finally, we display the result of checking if the 100th Fibonacci number is prime.

This code showcases a variety of mathematical functions and demonstrates how to use recursion and higher-order functions in Scheme.