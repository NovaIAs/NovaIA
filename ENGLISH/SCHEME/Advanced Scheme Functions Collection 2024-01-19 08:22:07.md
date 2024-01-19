```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (sum-of-squares n)
  (if (= n 0)
      0
      (+ (expt n 2) (sum-of-squares (- n 1)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (fibonacci n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define (sieve-of-eratosthenes n)
  (let loop ((primes (list)))
    (if (<= n 1)
        primes
        (let ((new-prime (car (cdr primes))))
          (loop (cons new-prime (filter (lambda (x)
                                          (not (= (remainder x new-prime) 0)))
                                          (cdr primes)))))))

(define (merge xs ys)
  (if (null? xs)
      ys
      (if (null? ys)
          xs
          (let ((x (car xs))
                (y (car ys)))
            (if (< x y)
                (cons x (merge (cdr xs) ys))
                (cons y (merge xs (cdr ys)))))))

(define (quicksort xs)
  (if (null? xs)
      '()
      (let ((pivot (car xs))
            (xs (cdr xs)))
        (merge (quicksort (filter (lambda (x) (< x pivot)) xs))
               (cons pivot
                    (quicksort (filter (lambda (x) (>= x pivot)) xs))))))

(define (hanoi n from to buf)
  (if (= n 0)
      '()
      (append (hanoi (- n 1) from buf to)
              (list (list from to))
              (hanoi (- n 1) buf to from))))

(define (queens n)
  (define (queen-safe row col queens)
    (and (>= row 0)
         (< row n)
         (>= col 0)
         (< col n)
         (not (memq (list row col) queens))))

  (define (queens-helper n row queens)
    (if (= n 0)
        (list queens)
        (let loop ((cols (iota n)))
          (if (null? cols)
              '()
              (let ((new-queen (list row (car cols))))
                (if (queen-safe row (car cols) queens)
                    (append (queens-helper n (+ row 1) (cons new-queen queens))
                            (loop (cdr cols)))))))))

  (queens-helper n 0 '()))

(define (knapsack items max-weight)
  (define (knapsack-helper items max-weight acc)
    (if (null? items)
        acc
        (let ((item (car items))
              (items (cdr items))
              (weight (car item))
              (value (cdr item)))
          (if (> weight max-weight)
              (knapsack-helper items max-weight acc)
              (let ((new-acc
                     (if (> (+ weight (car (cdr acc))) max-weight)
                         acc
                         (cons (cons item (car acc)) (cdr acc)))))
                (max (knapsack-helper items max-weight new-acc)
                    (knapsack-helper items (- max-weight weight)
                                     new-acc)))))))

  (knapsack-helper items max-weight '()))
```

This code contains several complex and differentiated functions, each of which performs a different task:

1. `factorial`: Computes the factorial of a non-negative integer.

2. `sum-of-squares`: Calculates the sum of squares of integers from 1 to a given non-negative integer.

3. `gcd`: Finds the greatest common divisor of two positive integers.

4. `fibonacci`: Computes the nth Fibonacci number.

5. `sieve-of-eratosthenes`: Generates a list of prime numbers up to a given positive integer using the Sieve of Eratosthenes algorithm.

6. `merge`: Merges two sorted lists into a single sorted list.

7. `quicksort`: Sorts a list of numbers using the quicksort algorithm.

8. `hanoi`: Generates a sequence of moves to solve the Tower of Hanoi puzzle.

9. `queens`: Finds all possible solutions to the N-queens puzzle.

10. `knapsack`: Solves the 0-1 knapsack problem, given a list of items with weights and values and a maximum weight capacity.

These functions demonstrate the versatility and expressiveness of the Scheme programming language, allowing you to perform complex tasks with concise and readable code.