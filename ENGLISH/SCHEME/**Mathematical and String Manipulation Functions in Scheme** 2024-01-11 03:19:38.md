```scheme
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fib n)
  (if (or (= n 0) (= n 1))
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (/ (* a b) (gcd a b)))

(define (is-prime? n)
  (if (<= n 1)
      #f
      (let loop ((i 2))
        (if (>= i (sqrt n))
            #t
            (if (= 0 (remainder n i))
                #f
                (loop (+ i 1)))))))

(define (prime-factors n)
  (cond
    [(is-prime? n) (list n)]
    [else
     (let loop ((prime-factors '()))
       (let ((factor (gcd n 2)))
         (if (= factor 1)
             prime-factors
             (let loop2 ((n (/ n factor)))
               (if (= 1 n)
                   (cons factor prime-factors)
                   (loop2 (gcd n 2)))))))]))

(define (gcd-list numbers)
  (if (null? numbers)
      0
      (let ((first (car numbers)))
        (foldl gcd first (cdr numbers)))))

(define (lcm-list numbers)
  (let ((product (foldl * 1 numbers)))
    (/ product (gcd-list numbers))))

(define (sum-of-digits n)
  (if (= n 0)
      0
      (+ (remainder n 10) (sum-of-digits (quotient n 10)))))

(define (reverse-string string)
  (if (null? string)
      ""
      (cons (last string) (reverse-string (butlast string)))))

(define (palindrome? string)
  (= string (reverse-string string)))

(define (count-occurrences string char)
  (if (null? string)
      0
      (if (= char (car string))
          (+ 1 (count-occurrences (cdr string) char))
          (count-occurrences (cdr string) char))))

(define (remove-duplicates string)
  (if (null? string)
      ""
      (let ((first (car string)))
        (if (count-occurrences string first)
            (remove-duplicates (cdr string))
            (cons first (remove-duplicates (cdr string)))))))

(define (shuffle string)
  (if (null? string)
      ""
      (let ((random-index (random (length string))))
        (cons (string-ref string random-index)
              (shuffle (delete string random-index 1))))))
```

This code implements a variety of mathematical and string manipulation functions in Scheme. Here's a brief explanation of each function:

1. `(fact n)`: Calculates the factorial of a non-negative integer `n`.
2. `(fib n)`: Computes the nth Fibonacci number.
3. `(gcd a b)`: Finds the greatest common divisor of two integers `a` and `b`.
4. `(lcm a b)`: Calculates the least common multiple of two integers `a` and `b`.
5. `(is-prime? n)`: Checks if a given integer `n` is prime.
6. `(prime-factors n)`: Returns a list of the prime factors of a given integer `n`.
7. `(gcd-list numbers)`: Computes the greatest common divisor of a list of integers.
8. `(lcm-list numbers)`: Calculates the least common multiple of a list of integers.
9. `(sum-of-digits n)`: Calculates the sum of the digits of a non-negative integer `n`.
10. `(reverse-string string)`: Reverses a given string.
11. `(palindrome? string)`: Checks if a given string is a palindrome (reads the same forward and backward).
12. `(count-occurrences string char)`: Counts the number of occurrences of a character in a given string.
13. `(remove-duplicates string)`: Removes duplicate characters from a given string.
14. `(shuffle string)`: Randomly shuffles the characters of a given string.

These functions demonstrate various programming concepts and techniques in Scheme, such as recursion, iteration, list manipulation, and string processing. They cover a wide range of mathematical and string-related tasks, making them useful in various scenarios.