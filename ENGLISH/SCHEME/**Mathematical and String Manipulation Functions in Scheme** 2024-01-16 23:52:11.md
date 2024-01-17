```scheme
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (fib n)
  (if (or (= n 0) (= n 1))
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (prime? n)
  (if (or (= n 1) (= n 2))
    #t
    (let loop ((i 2))
      (if (>= i (sqrt n))
        #t
        (if (= 0 (remainder n i))
          #f
          (loop (+ i 1)))))))

(define (next-prime n)
  (if (prime? (+ n 1))
    (+ n 1)
    (next-prime (+ n 1))))

(define (nth-prime n)
  (if (= n 1)
    2
    (next-prime (nth-prime (- n 1)))))

(define (is-palindrome? s)
  (let loop ((i 0) (j (- (string-length s) 1)))
    (if (>= i j)
      #t
      (if (not (char=? (string-ref s i) (string-ref s j)))
        #f
        (loop (+ i 1) (- j 1))))))

(define (reverse-string s)
  (let loop ((i 0) (j (- (string-length s) 1)) (r ""))
    (if (>= i j)
      r
      (loop (+ i 1) (- j 1) (string-append r (string-ref s j))))))

(define (count-occurrences s ch)
  (let loop ((i 0) (cnt 0))
    (if (>= i (string-length s))
      cnt
      (if (= (string-ref s i) ch)
        (loop (+ i 1) (+ cnt 1))
        (loop (+ i 1) cnt)))))

(define (remove-duplicates s)
  (let loop ((i 0) (r ""))
    (if (>= i (string-length s))
      r
      (if (not (string-contains? r (string-ref s i)))
        (loop (+ i 1) (string-append r (string-ref s i)))
        (loop (+ i 1) r)))))

(define (sort-string s)
  (let loop ((i 0) (r ""))
    (if (>= i (string-length s))
      r
      (let ((ch (string-ref s i)))
        (loop (+ i 1) (string-append r (string-insert (sort-string (substring s i)) ch 0)))))))

(define (anagram? s1 s2)
  (if (not (= (string-length s1) (string-length s2)))
    #f
    (let ((sorted-s1 (sort-string s1))
          (sorted-s2 (sort-string s2)))
      (= sorted-s1 sorted-s2))))

(define (all-anagrams s)
  (let loop ((i 0) (r '()))
    (if (>= i (string-length s))
      r
      (let ((ch (string-ref s i)))
        (loop (+ i 1) (cons (substring s i) (all-anagrams (substring s 0 i)))))))))
```

The above code contains various mathematical and string manipulation functions in Scheme. Here's a brief explanation of each function:

1. **Factorial:** This function calculates the factorial of a given number using recursion.

2. **Fibonacci:** This function calculates the nth Fibonacci number using recursion.

3. **Greatest Common Divisor (GCD):** This function calculates the greatest common divisor of two numbers using the Euclidean algorithm.

4. **Least Common Multiple (LCM):** This function calculates the least common multiple of two numbers using the GCD.

5. **Prime Number Check:** This function checks if a given number is prime.

6. **Next Prime:** This function finds the next prime number after a given number.

7. **Nth Prime:** This function finds the nth prime number.

8. **Palindrome Check:** This function checks if a given string is a palindrome.

9. **Reverse String:** This function reverses a given string.

10. **Count Occurrences:** This function counts the number of occurrences of a character in a given string.

11. **Remove Duplicates:** This function removes duplicate characters from a given string.

12. **Sort String:** This function sorts the characters of a given string in alphabetical order.

13. **Anagram Check:** This function checks if two given strings are anagrams of each other.

14. **All Anagrams:** This function finds all possible anagrams of a given string.

These functions cover a wide range of mathematical and string manipulation tasks and can be used in various applications.