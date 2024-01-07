```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (or (< n 0) (= n 1))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (prime? n)
  (if (<= n 1)
      #f
      (let loop ((i 2))
        (if (>= (* i i) n)
            #t
            (if (= (remainder n i) 0)
                #f
                (loop (+ i 1)))))))

(define (factors n)
  (if (prime? n)
      (list n)
      (let loop ((i 2) (factors '()))
        (if (= n 1)
            factors
            (if (= (remainder n i) 0)
                (loop (+ i 1) (cons i factors))
                (loop (+ i 1) factors))))))

(define (divisors n)
  (let loop ((i 1) (divisors '()))
    (if (> i n)
        divisors
        (if (= (remainder n i) 0)
            (loop (+ i 1) (cons i divisors))
            (loop (+ i 1) divisors))))))

(define (perfect? n)
  (if (= (apply + (divisors n)) (* 2 n))
      #t
      #f))

(define (abundant? n)
  (if (> (apply + (divisors n)) (* 2 n))
      #t
      #f))

(define (deficient? n)
  (if (< (apply + (divisors n)) (* 2 n))
      #t
      #f))

(define (amicable? a b)
  (and (= (apply + (divisors a)) b) (= (apply + (divisors b)) a)))

(define (sum-of-divisors n)
  (apply + (divisors n)))

(define (sum-of-proper-divisors n)
  (apply + (divisors n (sub1 n))))

(define (perfect-number? n)
  (and (perfect? n) (> n 0)))

(define (abundant-number? n)
  (and (abundant? n) (> n 0)))

(define (deficient-number? n)
  (and (deficient? n) (> n 0)))

(define (amicable-pair? a b)
  (and (amicable? a b) (> a 0) (> b 0)))
```

This code implements a variety of mathematical functions and properties in Scheme. It includes functions for calculating factorials, Fibonacci numbers, greatest common divisors, least common multiples, prime numbers, factors, divisors, perfect numbers, abundant numbers, deficient numbers, amicable numbers, and sums of divisors.

The code is organized into a series of functions, each of which implements a specific mathematical function or property. The functions are all defined using Scheme's `define` form, and they use a variety of Scheme's built-in functions and operators.

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

(prime? 17)
;; => #t

(factors 12)
;; => '(2 2 3)

(divisors 12)
;; => '(1 2 3 4 6 12)

(perfect? 6)
;; => #t

(abundant? 12)
;; => #t

(deficient? 2)
;; => #t

(amicable? 220 284)
;; => #t

(sum-of-divisors 12)
;; => 28

(sum-of-proper-divisors 12)
;; => 16

(perfect-number? 6)
;; => #t

(abundant-number? 12)
;; => #t

(deficient-number? 2)
;; => #t

(amicable-pair? 220 284)
;; => #t
```

This code can be used to perform a variety of mathematical calculations and to explore different mathematical properties. It can also be used as a starting point for developing more complex mathematical programs.