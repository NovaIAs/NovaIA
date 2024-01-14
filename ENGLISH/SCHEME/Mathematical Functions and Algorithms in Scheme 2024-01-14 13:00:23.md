```scheme
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (coprime? a b)
  (= (gcd a b) 1))

(define (prime? n)
  (cond ((<= n 1) #f)
        ((= n 2) #t)
        (else (and (not (even? n))
                   (for*/fold ((p 3 (+ p 2)))
                     ((> p (sqrt n)) #t)
                     (not (zero? (remainder n p)))))))

(define (next-prime n)
  (if (prime? n)
      n
      (next-prime (+ n 2))))

(define (factors n)
  (if (prime? n)
      '(n)
      (let loop ((n n) (factors '()))
        (if (= n 1)
            factors
            (let ((p (next-prime 1)))
              (if (= (remainder n p) 0)
                  (loop (/ n p) (cons p factors))
                  (loop n factors)))))))

(define (relatively-prime? a b)
  (and (coprime? a b)
       (coprime? a (lcm a b))))

(define (goldbach-pair n)
  (let loop ((n n) (p 2))
    (if (>= (+ p n) n)
        #f
        (if (and (prime? p) (prime? (- n p)))
            (list p (- n p))
            (loop n (next-prime (+ p 2)))))))

(define (cycle-length n)
  (let loop ((n n) (visited '()) (length 1))
    (if (memq n visited)
        length
        (loop (- n 1) (cons n visited) (+ length 1)))))

(define (collatz-sequence n)
  (let loop ((n n) (sequence '()))
    (if (= n 1)
        (reverse sequence)
        (if (even? n)
            (loop (/ n 2) (cons n sequence))
            (loop (* n 3) (cons n sequence))))))

(define (hailstone-sequence n)
  (let loop ((n n) (sequence '()))
    (if (= n 1)
        (reverse sequence)
        (if (odd? n)
            (loop (* n 3) (cons n sequence))
            (loop (/ n 2) (cons n sequence))))))

(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (tribonacci n)
  (if (<= n 2)
      n
      (+ (tribonacci (- n 1)) (tribonacci (- n 2)) (tribonacci (- n 3)))))

(define (lucas n)
  (if (<= n 1)
      n
      (+ (lucas (- n 1)) (lucas (- n 2)))))

(define (pell n)
  (if (<= n 1)
      n
      (+ (* 2 (pell (- n 1))) (pell (- n 2)))))

(define (catalan n)
  (if (= n 0)
      1
      (* n (+ (catalan (- n 1)) (catalan (- n 2))))))

(define (binomial n k)
  (if (or (< k 0) (> k n))
      0
      (/ (factorial n) (* (factorial (- n k)) (factorial k)))))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (harmonic-number n)
  (if (= n 0)
      0
      (+ 1 (/ 1 n) (harmonic-number (- n 1)))))

(define (zeta n)
  (if (= n 0)
      0
      (+ 1 (/ 1 (expt n n)) (zeta (- n 1)))))

(define (riemann-zeta s)
  (if (<= s 1)
      (harmonic-number (- 2 s))
      (/ (zeta s) (expt 2 (- 1 s)))))
```

This code implements a variety of mathematical functions and algorithms. It includes functions for calculating the greatest common divisor (gcd), least common multiple (lcm), and checking if two numbers are relatively prime. There's also a function for finding the next prime number greater than a given number, and a function for finding all the prime factors of a number.

The code also includes functions for checking if two numbers are coprime, and for finding a Goldbach pair for a given number. There are also functions for calculating the cycle length of a number under the Collatz conjecture, and for generating the Collatz sequence and the Hailstone sequence for a given number.

The code also includes functions for calculating the Fibonacci sequence, the Tribonacci sequence, the Lucas sequence, and the Pell sequence. There are also functions for calculating the Catalan numbers, the binomial coefficients, and the factorial of a number.

Finally, the code includes functions for calculating the harmonic number, the zeta function, and the Riemann zeta function.

This code is a good example of how Scheme can be used to implement a variety of mathematical functions and algorithms. It is also a good example of how Scheme can be used to write code that is both concise and expressive.