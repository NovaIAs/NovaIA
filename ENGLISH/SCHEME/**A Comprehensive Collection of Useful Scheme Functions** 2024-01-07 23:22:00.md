```scheme
(define (power n x)
  (if (= n 0)
      1
      (* x (power (- n 1) x))))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (/ (* a b) (gcd a b)))

(define (min3 a b c)
  (let ((m (min a b)))
    (min m c)))

(define (max3 a b c)
  (let ((m (max a b)))
    (max m c)))

(define (sum-of-squares n)
  (if (= n 0)
      0
      (+ (square n) (sum-of-squares (- n 1)))))

(define (fibonacci n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (isPrime n)
  (if (or (= n 2) (= n 3))
      True
      (and (not (even? n))
           (for-each (lambda (i) (not (= 0 (remainder n i))))
                     (range 3 (sqrt n) 2)))))

(define (reverse-list lst)
  (if (null? lst)
      '()
      (cons (car lst) (reverse-list (cdr lst)))))

(define (merge-sort lst)
  (if (null? lst)
      '()
      (let ((mid (quotient (length lst) 2)))
        (append (merge-sort (sublist lst 0 mid))
                (merge-sort (sublist lst mid (length lst)))))))

(define (quick-sort lst)
  (if (null? lst)
      '()
      (let ((pivot (car lst)))
        (append (quick-sort (filter (lambda (x) (< x pivot)) (cdr lst)))
                (list pivot)
                (quick-sort (filter (lambda (x) (>= x pivot)) (cdr lst)))))))

(define (binary-search lst x)
  (define (search low high)
    (if (> low high)
        -1
        (let ((mid (quotient (+ low high) 2)))
          (if (= (list-ref lst mid) x)
              mid
              (if (< x (list-ref lst mid))
                  (search low (- mid 1))
                  (search (+ mid 1) high))))))
  (search 0 (- (length lst) 1)))

(define (count-occurrences lst x)
  (if (null? lst)
      0
      (+ (if (= x (car lst)) 1 0)
         (count-occurrences (cdr lst) x))))

(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (remove-duplicates (remove (car lst) (cdr lst))))))

(define (flatten lst)
  (if (null? lst)
      '()
      (append (if (pair? (car lst))
                  (flatten (car lst))
                  (list (car lst)))
              (flatten (cdr lst)))))
```

This code contains a large number of useful functions, including:

* `power`: Raises a number to a power.
* `factorial`: Calculates the factorial of a number.
* `gcd`: Finds the greatest common divisor of two numbers.
* `lcm`: Finds the least common multiple of two numbers.
* `min3`: Finds the minimum of three numbers.
* `max3`: Finds the maximum of three numbers.
* `sum-of-squares`: Calculates the sum of the squares of the first n natural numbers.
* `fibonacci`: Calculates the nth Fibonacci number.
* `isPrime`: Checks if a number is prime.
* `reverse-list`: Reverses the order of the elements in a list.
* `merge-sort`: Sorts a list using the merge sort algorithm.
* `quick-sort`: Sorts a list using the quick sort algorithm.
* `binary-search`: Searches for an element in a sorted list using the binary search algorithm.
* `count-occurrences`: Counts the number of occurrences of an element in a list.
* `remove-duplicates`: Removes duplicate elements from a list.
* `flatten`: Flattens a nested list into a single-level list.

These functions can be used for a wide variety of tasks, from simple calculations to complex data analysis.