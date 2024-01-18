```scheme
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))

(define (fibonacci n)
  (if (or (zero? n) (zero? (sub1 n)))
      1
      (+ (fibonacci (sub1 n)) (fibonacci (sub2 n)))))

(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (is-prime? n)
  (if (or (<= n 1) (zero? (remainder n 2)))
      #f
      (let loop ((i 3))
        (if (> i (sqrt n))
            #t
            (if (zero? (remainder n i))
                #f
                (loop (+ i 2)))))))

(define (next-prime n)
  (define (next-prime-helper n)
    (if (is-prime? n)
        n
        (next-prime-helper (+ n 1))))
  (if (is-prime? n)
      n
      (next-prime-helper (+ n 1))))

(define (prime-factors n)
  (define (prime-factors-helper n factors)
    (if (is-prime? n)
        (cons n factors)
        (let ((p (next-prime 2)))
          (if (zero? (remainder n p))
              (prime-factors-helper (/ n p) (cons p factors))
              (prime-factors-helper n factors)))))
  (if (is-prime? n)
      (list n)
      (reverse (prime-factors-helper n '()))))

(define (combinations n r)
  (if (or (< n 0) (< r 0) (> r n))
      0
      (let ((numerator (factorial n))
            (denominator (* (factorial r) (factorial (- n r)))))
        (/ numerator denominator))))

(define (permutations n r)
  (if (or (< n 0) (< r 0) (> r n))
      0
      (* (factorial n) (/ 1 (factorial (- n r))))))

(define (binomial-coefficient n k)
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

(define (catalan-number n)
  (/ (binomial-coefficient (* 2 n) n) (add1 n)))

(define (bell-number n)
  (if (zero? n)
      1
      (let loop ((i 1)
                 (sum 0))
        (if (> i n)
            sum
            (loop (+ i 1) (+ sum (* (bell-number (- n i)) i)))))))

(define (stirling-number-of-the-second-kind n k)
  (if (or (< n 0) (< k 0) (> k n))
      0
      (cond
        [(zero? k) 1]
        [(= k n) 1]
        [(zero? n) 0]
        [else (+ (* (-1) k) (* (stirling-number-of-the-second-kind (- n 1) k) n)))])))
```

This code is a collection of common mathematical functions, including factorial, fibonacci, greatest common divisor (gcd), least common multiple (lcm), primality testing, prime factorization, combinations, permutations, binomial coefficients, Catalan numbers, Bell numbers, and Stirling numbers of the second kind. Each function is explained in the comments.