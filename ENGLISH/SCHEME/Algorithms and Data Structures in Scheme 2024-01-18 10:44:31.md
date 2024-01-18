```scheme
(define (fibonacci n)
  (cond
    [(<= n 1) n]
    [else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))]))

(define (sum-of-squares n)
  (if (= n 0)
      0
      (+ (square n) (sum-of-squares (- n 1)))))

(define (square n)
  (* n n))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (prime? n)
  (if (<= n 1)
      #f
      (cond
        [(= n 2) #t]
        [(= (remainder n 2) 0) #f]
        [else (for-each (lambda (d) (if (= (remainder n d) 0) #f #t)) (range 3 (sqrt n) 2))])))

(define (sieve-of-eratosthenes n)
  (define (mark-multiples d ls)
    (for-each (lambda (i) (set! (ref ls i) #f)) (range (* d d) n d)))

  (make-vector n #t)
  (set! (ref (vector-ref ls 0)) #f)
  (set! (ref (vector-ref ls 1)) #f)
  (for-each (lambda (d) (if (vector-ref ls d) (mark-multiples d ls))) (range 2 (sqrt n)))
  (list->vector (filter vector-ref ls)))

(define (next-permutation ls)
  (define (swap i j)
    (let ((temp (vector-ref ls i)))
      (vector-set! ls i (vector-ref ls j))
      (vector-set! ls j temp)))

  (let ((n (vector-length ls)))
    (if (>= n 2)
        (let loop ((i (- n 2)))
          (if (vector-ref ls i) (> (vector-ref ls i) (vector-ref ls (+ i 1)))))
            (loop (- i 1))
            (if (= i -1)
                #f
                (let ((j (+ i 1)))
                  (loop (+ j 1)
                    (if (> (vector-ref ls j) (vector-ref ls i)))
                        (j)
                        (loop (+ j 1))))
                  (swap i j)
                  (reverse! ls (1+ i) n)
                  #t))))))))

(define (combinations n k)
  (define (loop acc i)
    (if (= i k)
        (cons acc '())
        (append
          (loop (cons (vector-ref ls i) acc) (+ i 1))
          (loop acc (+ i 1)))))

  (make-vector n (iota n))
  (loop '() 0))

(define (permutations n)
  (for-each (lambda (ls) (display ls)) (combinations n n)))

(define (subsets n)
  (for-each (lambda (ls) (display ls)) (combinations n 0)))

(define (powerset n)
  (for-each (lambda (ls) (display ls)) (subsets n)))

(define (knapsack items capacity)
  (define (solve i w p)
    (cond
      [(= i (length items)) 0]
      [(> w capacity) -1]
      [else (max
              (solve (+ i 1) w p)
              (+ (item-value (vector-ref items i))
                 (solve (+ i 1) (+ w (item-weight (vector-ref items i)))
                        (+ p (item-profit (vector-ref items i))))))]))

  (sort items (lambda (a b) (> (item-profit a) (item-profit b))))
  (solve 0 0 0))

(define (traveling-salesman cities)
  (define (solve visited)
    (cond
      [(= (length visited) (length cities)) (distance (first visited) (first cities))]
      [else (min (for-each (lambda (c)
                             (if (member c visited)
                                 infinity
                                 (+ (solve (cons c visited))
                                    (distance (last visited) c))))
                          cities))]))

  (solve '()))
```
This code is a collection of various useful algorithms and data structures implemented in Scheme. Here's a brief explanation of each function:

1. **fibonacci:** Calculates the nth Fibonacci number using recursion.

2. **sum-of-squares:** Finds the sum of squares of integers from 1 to `n`.

3. **square:** Simply squares a given number.

4. **factorial:** Calculates the factorial of a given number.

5. **gcd:** Computes the greatest common divisor (GCD) of two numbers using Euclid's algorithm.

6. **lcm:** Calculates the least common multiple (LCM) of two numbers.

7. **prime?:** Checks if a given number is prime.

8. **sieve-of-eratosthenes:** Generates a list of prime numbers up to `n` using the Sieve of Eratosthenes algorithm.

9. **next-permutation:** Finds the next permutation of a list in lexicographic order.

10. **combinations:** Generates all combinations of `k` elements from a list of `n` elements.

11. **permutations:** Generates all permutations of `n` elements.

12. **subsets:** Generates all subsets of a list of `n` elements.

13. **powerset:** Generates the power set of a list of `n` elements, which is the set of all subsets of the list.

14. **knapsack:** Solves the 0/1 knapsack problem using dynamic programming. Given a set of items with weights and values, and a knapsack with a maximum capacity, it finds the maximum total value of items that can be put into the knapsack without exceeding its capacity.

15. **traveling-salesman:** Solves the traveling salesman problem using a brute-force approach. Given a list of cities and the distances between them, it finds the shortest possible tour that visits each city exactly once and returns to the starting city.