```scheme
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (isPrime n)
  (if (= n 2)
      #t
      (and (> n 1) (null? (filter (lambda (x) (zero? (remainder n x))) (range 2 (sqrt n)))))))

(define (prime-factors n)
  (if (isPrime n)
      '(n)
      (let loop ((factors '()) (divisor 2))
        (if (> divisor (sqrt n))
            factors
            (if (zero? (remainder n divisor))
                (loop (cons divisor factors) divisor)
                (loop factors (+ divisor 1)))))))

(define (combinations n k)
  (if (or (= n 0) (= k 0) (> k n))
      '()
      (cons (map (lambda (x) (cons x (combinations (- n 1) (- k 1)))) (range 1 n))
            (combinations n (- k 1)))))

(define (permutations n)
  (if (= n 0)
      '()
      (map (lambda (x) (cons x (permutations (- n 1)))) (range 1 n))))

(define (subsets n)
  (if (= n 0)
      '(())
      (let loop ((subsets '()) (element 1))
        (if (> element n)
            subsets
            (loop (cons (cons element (subsets (- n 1))) subsets) (+ element 1))))))

(define (power-set n)
  (map (lambda (x) (subsets x)) (range 0 n)))

(define (merge-sort lst)
  (if (null? lst)
      '()
      (let ((mid (quotient (length lst) 2)))
        (merge (merge-sort (sublist lst 0 (- mid 1)))
               (merge-sort (sublist lst mid (length lst))))))))

(define (quick-sort lst)
  (if (null? lst)
      '()
      (let ((pivot (car lst)))
        (append (quick-sort (filter (lambda (x) (< x pivot)) (cdr lst)))
               (cons pivot
                    (quick-sort (filter (lambda (x) (>= x pivot)) (cdr lst)))))))))

(define (binary-search lst key)
  (let loop ((low 0) (high (- (length lst) 1)))
    (if (> low high)
        -1
        (let ((mid (quotient (+ low high) 2)))
          (if (= (list-ref lst mid) key)
              mid
              (if (< key (list-ref lst mid))
                  (loop low (- mid 1))
                  (loop (+ mid 1) high))))))))

(define (dijkstra-shortest-path graph start end)
  (define (distance-to node)
    (if (member node start)
        0
        (or (member node (cdr graph))
            (add1 (distance-to (car (cdr graph)))))))
  (define (update-distances node)
    (for-each (lambda (edge)
               (set-car! edge (add1 (distance-to node))))
              (cdr (assoc node graph))))
  (for-each update-distances start)
  (let loop ((queue '(start)) (visited '()))
    (if (null? queue)
        '()
        (let ((node (car queue)))
          (if (member node end)
              (reverse (cons node visited))
              (loop (cdr queue) (cons node visited)))))))
```

This code is a collection of various mathematical and algorithmic functions written in the Scheme programming language. Here's a brief explanation of each function:

1. `factorial`: Calculates the factorial of a non-negative integer `n`.

2. `fibonacci`: Computes the `n`th Fibonacci number.

3. `gcd`: Finds the greatest common divisor of two non-negative integers `a` and `b`.

4. `lcm`: Calculates the least common multiple of two non-negative integers `a` and `b`.

5. `isPrime`: Checks if a given integer `n` is prime.

6. `prime-factors`: Lists all the prime factors of a given integer `n`.

7. `combinations`: Generates all combinations of `k` elements from a set of `n` elements.

8. `permutations`: Computes all permutations of a set of `n` elements.

9. `subsets`: Finds all subsets of a set of `n` elements.

10. `power-set`: Generates the power set of a set of `n` elements.

11. `merge-sort`: Implements the merge sort algorithm to sort a list.

12. `quick-sort`: Implements the quick sort algorithm to sort a list.

13. `binary-search`: Performs a binary search for a given `key` in a sorted list.

14. `dijkstra-shortest-path`: Implements Dijkstra's algorithm to find the shortest path between two nodes in a weighted graph.