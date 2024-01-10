```scheme
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (< n 2)
    n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (isPrime n)
  (define (isPrimeHelp n d)
    (if (= d 1)
      #t
      (or (= (remainder n d) 0) (isPrimeHelp n (- d 1)))))
  (isPrimeHelp n (- n 1)))

(define (nextPrime n)
  (if (isPrime (1+ n)) (1+ n) (nextPrime (1+ n))))

(define (nthPrime n)
  (define (nthPrimeHelp n p c)
    (if (= c n)
      p
      (nthPrimeHelp n (nextPrime p) (1+ c))))
  (nthPrimeHelp n 2 1))

(define (sieveOfEratosthenes n)
  (define (sieveHelp n l)
    (if (null? l)
      '()
      (cons (car l) (sieveHelp n (filter (lambda (x) (>= x (car l))) (cdr l))))))
  (sieveHelp n (range 2 (1+ n))))

(define (merge l1 l2)
  (define (mergeHelp l1 l2 l)
    (if (null? l1)
      (append l2 l)
      (if (null? l2)
        (append l1 l)
        (if (< (car l1) (car l2))
          (mergeHelp (cdr l1) l2 (cons (car l1) l))
          (mergeHelp l1 (cdr l2) (cons (car l2) l))))))
  (mergeHelp l1 l2 '()))

(define (mergeSort l)
  (if (null? l)
    '()
    (if (null? (cdr l))
      l
      (let ((mid (/ (length l) 2)))
        (merge (mergeSort (sublist l 0 mid)) (mergeSort (sublist l mid (length l)))))))))

(define (quickSort l)
  (if (null? l)
    '()
    (let ((pivot (car l)))
      (append (quickSort (filter (lambda (x) (< x pivot)) (cdr l)))
             (cons pivot (quickSort (filter (lambda (x) (>= x pivot)) (cdr l)))))))))

(define (heapSort l)
  (define (heapify l i)
    (let ((l (length l)))
      (if (and (< (* 2 i) l) (<= (car l) (car (sublist l (* 2 i) 1))))
        (let ((temp (car l)))
          (set-car! l (car (sublist l (* 2 i) 1)))
          (set-car! (sublist l (* 2 i) 1) temp)
          (heapify l (* 2 i)))
      (if (and (< (+ (* 2 i) 1) l) (<= (car l) (car (sublist l (+ (* 2 i) 1) 1))))
        (let ((temp (car l)))
          (set-car! l (car (sublist l (+ (* 2 i) 1) 1)))
          (set-car! (sublist l (+ (* 2 i) 1) 1) temp)
          (heapify l (+ (* 2 i) 1))))))))
  (define (buildHeap l)
    (for-each (lambda (i) (heapify l i)) (range (floor (/ (length l) 2)) 0 -1)))
  (define (heapSortHelp l)
    (if (null? l)
      '()
      (let ((temp (car l)))
        (set-car! l (car (sublist l (length l) -1)))
        (set-car! (sublist l (length l) -1) temp)
        (heapify l 1)
        (cons (car l) (heapSortHelp (sublist l 0 (length l) -1))))))
  (buildHeap l)
  (heapSortHelp l))

(define (countingSort l)
  (define (max l)
    (if (null? (cdr l))
      (car l)
      (max (cdr l) (car l))))
  (let ((m (max l)))
    (define (countingSortHelp l c)
      (if (null? l)
        '()
        (cons (car l) (countingSortHelp (cdr l) (set! (sublist c (car l) 1) (1+ (car (sublist c (car l) 1)))))))))
    (countingSortHelp l (make-vector (1+ m) 0))))
```

This code contains a collection of mathematical and sorting algorithms implemented in Scheme. Here's a brief explanation of each function:

1. `factorial`: Calculates the factorial of a given number.

2. `fibonacci`: Computes the nth Fibonacci number using recursion.

3. `gcd`: Finds the greatest common divisor of two numbers using Euclid's algorithm.

4. `lcm`: Calculates the least common multiple of two numbers.

5. `isPrime`: Checks if a given number is prime.

6. `nextPrime`: Finds the next prime number greater than a given number.

7. `nthPrime`: Returns the nth prime number.

8. `sieveOfEratosthenes`: Generates a list of prime numbers up to a given limit using the Sieve of Eratosthenes algorithm.

9. `merge`: Merges two sorted lists into a single sorted list.

10. `mergeSort`: Sorts a list using the merge sort algorithm.

11. `quickSort`: Sorts a list using the quick sort algorithm.

12. `heapSort`: Sorts a list using the heap sort algorithm.

13. `countingSort`: Sorts a list of integers using the counting sort algorithm.

These functions provide a variety of mathematical and sorting capabilities that can be useful in various programming scenarios.