```scheme
(define (pascal-triangle n)
; Base case: n = 0.
  (if (= n 0) '((1)))
; Recursive case: n > 0.
  (let loop ([n n] [acc '(() (1))])
    (if (= n 0) acc
      (loop (- n 1) (append acc (map (lambda (x y) (+ x y))
                                      (cdr acc)
                                      (cdr (cdr acc)))))))

(define (euler-number n)
  (let loop ([n n] [acc 0])
    (if (= n 0) acc
      (loop (- n 1) (+ acc (/ 1 (- (expt 2 n) 1))))))

(define (fib n)
  (if (= n 0) 0
    (if (= n 1) 1
      (+ (fib (- n 1)) (fib (- n 2))))))

(define (gcd a b)
  (if (= b 0) a
    (gcd b (remainder a b))))

(define (lcm a b)
  (let lcm (/ (* a b) (gcd a b)))
  (if (= lcm 0) 1 lcm))

(define (is-prime? n)
  (if (= n 2) #t
    (if (= n 1) #f
      (let loop ([n n] [i 2])
        (if (= i (sqrt (+ n 1))) #t
          (if (= (remainder n i) 0) #f
            (loop n (+ i 1)))))))

(define (prime-factors n)
  (let loop ([n n] [acc '()])
    (if (= n 1) acc
      (let prime (first (filter (lambda (p) (= 0 (remainder n p)))
                                (range 2 (+ n 1))))
        (loop (/ n prime) (cons prime acc))))))

(define (sieve-of-eratosthenes n)
  (let loop ([n n] [primes '()])
    (if (= n 2) primes
      (let new-primes
            (cons 2
                  (filter (lambda (p) (not (any? (lambda (x) (= 0 (remainder p x))) primes)))
                          (range 3 (+ n 2) 2)))
        (loop (/ n 2) (append primes new-primes))))))

(define (merge-sort xs)
  (let loop ([xs xs] [ys '()])
    (if (null? xs) ys
      (let ys (cons (first xs) ys)
          xs (cdr xs)
        (loop xs (merge ys (loop xs '())))))))

(define (merge xs ys)
  (if (or (null? xs) (null? ys))
    (append xs ys)
    (let x (first xs)
        y (first ys)
    (if (< x y)
      (cons x (merge (cdr xs) ys))
      (cons y (merge xs (cdr ys)))))))

(define (quick-sort xs)
  (if (null? xs) '()
    (let pivot (first xs)
        less-than (filter (lambda (x) (< x pivot)) (cdr xs))
        greater-than (filter (lambda (x) (> x pivot)) (cdr xs))
    (append (quick-sort less-than) (cons pivot) (quick-sort greater-than)))))

(define (radix-sort xs)
  (let loop ([xs xs] [exp 1])
    (if (= exp 0) xs
      (let buckets (make-vector 10)
          xs (for-each (lambda (x)
                        (vector-set! buckets (remainder x exp) (cons x (vector-ref buckets (remainder x exp)))))
                      xs)
          xs (reverse (for-each (lambda (i) (reverse (vector-ref buckets i)))
                               (range 10)))
        (loop xs (* exp 10))))))

(define (heap-sort xs)
  (let* ([n (length xs)]
         [heap (vector (length xs))])
    (for-each (lambda (x)
                (heap-insert! heap x))
              xs)
    (for-each (lambda (i)
                (vector-set! heap i (heap-remove-max! heap)))
              (range (- n 1) 0 -1))
    (vector->list heap))))

(define (heap-insert! heap x)
  (let* ([i (length heap)]
         [parent (/ (- i 1) 2)])
    (vector-set! heap i x)
    (while (and (> i 0) (< (vector-ref heap i) (vector-ref heap parent)))
      (let tmp (vector-ref heap i))
          (vector-set! heap i (vector-ref heap parent))
          (vector-set! heap parent tmp)
          (set! i parent)
          (set! parent (/ (- i 1) 2)))))

(define (heap-remove-max! heap)
  (let* ([max (vector-ref heap 0)]
         [i 0]
         [l 1]
         [r 2])
    (vector-set! heap 0 (vector-ref heap (length heap)))
    (vector-pop! heap)
    (while (and (< i (length heap))
               (or (> (vector-ref heap l) max) (> (vector-ref heap r) max)))
      (if (> (vector-ref heap l) (vector-ref heap r))
        (let tmp (vector-ref heap i))
            (vector-set! heap i (vector-ref heap l))
            (vector-set! heap l tmp)
            (set! i l)
            (set! l (* 2 (+ 1 i))))
      (let tmp (vector-ref heap i))
          (vector-set! heap i (vector-ref heap r))
          (vector-set! heap r tmp)
          (set! i r)
          (set! r (* 2 (+ 1 i)))))
    max)))
```

This code includes a collection of common algorithms and mathematical functions implemented in Scheme. Each function is accompanied by a brief explanation of its purpose and how it works. Here's a breakdown of each function:

1. **pascal-triangle**: Generates Pascal's triangle up to a specified number of rows.

2. **euler-number**: Calculates the Euler number for a given integer.

3. **fib**: Computes the Fibonacci sequence up to a specified index.

4. **gcd**: Finds the greatest common divisor of two integers.

5. **lcm**: Calculates the least common multiple of two integers.

6. **is-prime?**: Determines if a given integer is prime.

7. **prime-factors**: Lists all the prime factors of a given integer.

8. **sieve-of-eratosthenes**: Generates a list of prime numbers up to a specified limit using the Sieve of Eratosthenes algorithm.

9. **merge-sort**: Sorts a list of integers using the merge sort algorithm.

10. **merge**: Merges two sorted lists into a single sorted list.

11. **quick-sort**: Sorts a list of integers using the quick sort algorithm.

12. **radix-sort**: Sorts a list of integers using the radix sort algorithm.

13. **heap-sort**: Sorts a list of integers using the heap sort algorithm.

14. **heap-insert!**: Inserts an element into a heap data structure while maintaining the heap property.

15. **heap-remove-max!**: Removes the maximum element from a heap data structure while maintaining the heap property.