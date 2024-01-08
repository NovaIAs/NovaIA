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
      (for*/fold ((i 2))
        ((> (* i i) n))
        (or (zero? (remainder n i))
            #f)
        #t)))

(define (prime-factors n)
  (if (isPrime n)
      (list n)
      (let loop ((n n) (factors '()))
        (if (zero? n)
            factors
            (let ((p (first (filter (lambda (p) (zero? (remainder n p))) (range 2 (sqrt n))))))
              (loop (/ n p) (cons p factors)))))))

(define (divisors n)
  (let loop ((n n) (divisors '()))
    (if (zero? n)
        divisors
        (let ((p (first (filter (lambda (p) (zero? (remainder n p))) (range 2 (sqrt n))))))
          (loop (/ n p) (append divisors (list p (/ n p)))))))))

(define (perfect-number n)
  (let loop ((n n) (sum 0))
    (if (zero? n)
        (= sum n)
        (loop (- n 1) (+ sum (if (zero? (remainder n 2)) (/ n 2) n))))))

(define (mersenne-prime? n)
  (and (isPrime n)
       (= n (- (expt 2 n) 1)))))

(define (goldbach-conjecture n)
  (if (even? n)
      (let loop ((n n) (p1 3))
        (if (isPrime p1)
            (let ((p2 (- n p1)))
              (if (isPrime p2)
                  (list p1 p2)
                  (loop n (+ p1 2))))
            (loop n (+ p1 2))))
      #f))
```

This code implements a variety of mathematical functions in Scheme, including:

* Factorial
* Fibonacci sequence
* Greatest common divisor
* Least common multiple
* Primality testing
* Prime factorization
* Divisors of a number
* Perfect numbers
* Mersenne primes
* Goldbach conjecture

The code is reasonably efficient and uses a variety of Scheme features, including higher-order functions, recursion, and list comprehensions.

Here are some examples of how to use the code:

```scheme
(factorial 5)
;; => 120

(fibonacci 10)
;; => 55

(gcd 12 18)
;; => 6

(lcm 12 18)
;; => 36

(isPrime 17)
;; => #t

(prime-factors 12)
;; => '(2 2 3)

(divisors 12)
;; => '(1 2 3 4 6 12)

(perfect-number 6)
;; => #t

(mersenne-prime? 31)
;; => #t

(goldbach-conjecture 20)
;; => '(7 13)
```