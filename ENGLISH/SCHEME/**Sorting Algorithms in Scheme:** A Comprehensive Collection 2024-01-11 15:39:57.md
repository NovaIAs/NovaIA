```scheme
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (prime? n)
  (cond
    [(= n 2) #t]
    [(or (even? n) (= n 1)) #f]
    [else (for/list ([i (in-range 3 (sqrt n) 2)])
           (not (zero? (remainder n i))))]))

(define (next-prime n)
  (if (prime? n)
      n
      (next-prime (+ n 1))))

(define (sieve-of-eratosthenes n)
  (define primes (list))
  (for ([i (in-range 2 n)])
    (when (prime? i)
      (add-to-list! primes i)))
  primes)

(define (nth-prime n)
  (apply nth (sieve-of-eratosthenes n) (- n 1)))

(define (merge-sort xs)
  (cond
    [(empty? xs) '()]
    [(= (length xs) 1) xs]
    [else (let ([mid (quotient (length xs) 2)])
           (append
             (merge-sort (sublist xs 0 mid))
             (merge-sort (sublist xs mid (length xs)))))]))

(define (quicksort xs)
  (cond
    [(empty? xs) '()]
    [(= (length xs) 1) xs]
    [else (let ([pivot (first xs)])
           (append
             (quicksort (filter (lambda (x) (< x pivot)) (rest xs)))
             (list pivot)
             (quicksort (filter (lambda (x) (>= x pivot)) (rest xs)))))]))

(define (heap-sort xs)
  (define heap (heapify xs))
  (for/list ([i (in-range 1 (length xs))])
    (let* ([max (first heap)])
          (set-car! heap (last heap))
          (set-cdr! heap (remove max (cdr heap)))
          (insert! heap max)
          max)))

(define (heapify xs)
  (if (empty? xs)
      '()
      (let* ([n (length xs)])
            (for/list ([i (in-range 1 n)])
              (sift-down! xs i n)))))

(define (sift-down! xs i n)
  (define child (* i 2))
  (if (>= child n)
      xs
      (let* ([max-child (max-child-index xs child (1+ child) n)])
            (swap! xs i max-child)
            (sift-down! xs max-child n))))

(define (max-child-index xs i j n)
  (if (>= j n)
      i
      (let ([k (+ j 1)])
        (if (>= k n)
            (if (> (car xs j) (car xs i)) j i)
            (let* ([max-child (max-child-index xs j k n)])
              (if (> (car xs max-child) (car xs i))
                  max-child
                  i))))))

(define (insertion-sort xs)
  (for ([i (in-range 2 (length xs))])
    (let* ([x (car (sublist xs i (length xs)))]
          (j (- i 1)))
      (while (and (> j -1) (> x (car (sublist xs j 1))))
        (set-car! (sublist xs (+ j 1) (length xs)) (car (sublist xs j 1)))
        (set! j (- j 1)))
      (set-car! (sublist xs (+ j 1) (length xs)) x))))

(define (selection-sort xs)
  (for ([i (in-range 0 (sub1 (length xs))))
    (let* ([min-index i])
      (for ([j (in-range (+ i 1) (length xs))])
        (if (< (car (sublist xs j 1)) (car (sublist xs min-index 1)))
            (set! min-index j)))
      (swap! xs i min-index))))

(define (bubble-sort xs)
  (let loop ([xs xs]
             [swapped? #f])
    (cond
      [(empty? xs) '()]
      [(= (length xs) 1) xs]
      [else (let ([y (cdr xs)])
             (for ([i (in-range 0 (sub1 (length y))))
               (if (> (car y i) (car y (+ i 1)))
                   (begin
                     (swap! y i (+ i 1))
                     (set! swapped? #t))
                   #f))
             (if swapped?
                 (loop y #t)
                 (cons (car xs) (loop y #f))))])))

(define (radix-sort xs)
  (define max-digit (apply max xs))
  (define exp 1)
  (while (> max-digit (/ exp 10))
    (counting-sort xs exp)
    (set! exp (* exp 10)))
  xs)

(define (counting-sort xs exp)
  (define n (length xs))
  (define output (make-vector n))
  (define count (make-vector 10))
  (for ([i (in-range 0 n)])
    (let ([index (quotient (car xs i) exp)])
      (add-to! (vector-ref count index) 1)))
  (for ([i 1 10])
    (set! (vector-ref count i) (+ (vector-ref count i) (vector-ref count (- i 1)))))
  (for ([i (in-range (sub1 n) -1 -1)])
    (let ([index (quotient (car xs i) exp)])
      (set-vector! output (- (vector-ref count index) 1) (car xs i))
      (sub1! (vector-ref count index))))
  (for ([i (in-range 0 n)])
    (set-car! xs (vector-ref output i))))

(define (bucket-sort xs)
  (define n (length xs))
  (define buckets (make-vector n))
  (for ([i (in-range 0 n)])
    (vector-set! buckets (quotient (car xs i) n) (cons (car xs i) (vector-ref buckets (quotient (car xs i) n)))))
  (for ([i (in-range 0 n)])
    (set-car! xs (car (vector-ref buckets i))))
  (for ([i (in-range 0 n)])
    (set-car! xs (car (cdr (vector-ref buckets i))))))
```

This code contains a collection of various sorting algorithms implemented in Scheme, including:

1. **Factorial:** Calculates the factorial of a given integer.

2. **Fibonacci:** Computes the nth Fibonacci number.

3. **Greatest Common Divisor (GCD):** Finds the greatest common divisor of two integers.

4. **Least Common Multiple (LCM):** Computes the least common multiple of two integers.

5. **Primality Test:** Checks if a given integer is prime.

6. **Next Prime:** Finds the next prime number after a given integer.

7. **Sieve of Eratosthenes:** Generates a list of prime numbers up to a given limit.

8. **Nth Prime:** Finds the nth prime number.

9. **Merge Sort:** Sorts a list using the merge sort algorithm.

10. **Quicksort:** Sorts a list using the quicksort algorithm.

11. **Heap Sort:** Sorts a list using the heap sort algorithm.

12. **Heapify:** Converts a list into a heap data structure.

13. **Sift Down:** Maintains the heap property in a heap data structure.

14. **Max Child Index:** Finds the index of the child with the maximum value in a heap.

15. **Insertion Sort:** Sorts a list using the insertion sort algorithm.

16. **Selection Sort:** Sorts a list using the selection sort algorithm.

17. **Bubble Sort:** Sorts a list using the bubble sort algorithm.

18. **Radix Sort:** Sorts a list of integers using