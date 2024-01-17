```scheme
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* (/ (* a b) (gcd a b)) 2))

(define (is-prime? n)
  (if (= n 1)
      #f
      (let loop ([i 2])
        (if (>= i (sqrt n))
            #t
            (cond
              [(= 0 (remainder n i)) #f]
              [else (loop (add1 i)]))))))

(define (prime-factors n)
  (if (is-prime? n)
      (list n)
      (cons (car (filter (lambda (p) (= 0 (remainder n p)))
                         (range 2 (sqrt n))))
            (prime-factors (/ n (car (filter (lambda (p) (= 0 (remainder n p)))
                                         (range 2 (sqrt n))))))))

(define (prime-factorization n)
  (append (car (prime-factors n))
          (prime-factorization (cadr (prime-factors n)))))

(define (count-divisors n)
  (let loop ([factors (prime-factorization n)]
             [divisors 1])
    (if (null? factors)
        divisors
        (let* ([prime (car factors)]
               [exp (cadr factors)])
          (loop (cdr factors)
                (* divisors (+ 1 (expt prime exp))))))))

(define (perfect-number? n)
  (let divisors (count-divisors n))
    (= n (* divisors 2))))

(define (smallest-perfect-number)
  (let loop ([i 2])
    (if (perfect-number? i)
        i
        (loop (add1 i)))))

(display (smallest-perfect-number))
```

This code is a collection of functions that work with numbers, including finding the greatest common divisor (gcd), least common multiple (lcm), and prime factors of a number.

It also includes functions to check if a number is prime, count the divisors of a number, and determine if a number is perfect (the sum of its proper divisors is equal to the number itself).

The final function, `smallest-perfect-number`, uses the previous functions to find the smallest perfect number, which is the first perfect number greater than 1.