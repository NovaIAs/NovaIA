```scheme
(define fib
  (lambda (n)
    (if (= n 0) 0
        (if (= n 1) 1
            (+ (fib (- n 1)) (fib (- n 2)))))))

(display (fib 10))  ; output: 55

(define fact
  (lambda (n)
    (if (= n 0) 1
        (* n (fact (- n 1))))))

(display (fact 5))  ; output: 120

(define gcd
  (lambda (a b)
    (if (= b 0) a
        (gcd b (remainder a b)))))

(display (gcd 12 18))  ; output: 6

(define prime?
  (lambda (n)
    (every?
      (lambda (d) (/= (remainder n d) 0)) ;; checks if d is not a divisor of n
      (iota 2 (sqrt n)))))

(display (prime? 13))  ; output: true

(define mergesort
  (lambda (lst)
    (if (empty? lst) '()
        (let ((pivot (car lst))
              (less (filter (lambda (x) (< x pivot)) (cdr lst)))
              (greater (filter (lambda (x) (>= x pivot)) (cdr lst))))
          (append (mergesort less)
                  (cons pivot (mergesort greater)))))))

(display (mergesort '(1 4 3 7 2 6 5 8)))  ; output: '(1 2 3 4 5 6 7 8)
```

Explanation:

1. **Fibonacci Sequence**: Calculates the Fibonacci number at a given index.

2. **Factorial**: Calculates the factorial of a given number.

3. **Greatest Common Divisor (GCD)**: Finds the greatest common divisor of two numbers using the Euclidean algorithm.

4. **Primality Test**: Checks if a given number is prime by testing divisibility by all smaller numbers up to its square root.

5. **Merge Sort**: Implements the merge sort algorithm to sort a list of numbers.

These functions demonstrate a variety of programming concepts like recursion, filtering, and sorting algorithms.