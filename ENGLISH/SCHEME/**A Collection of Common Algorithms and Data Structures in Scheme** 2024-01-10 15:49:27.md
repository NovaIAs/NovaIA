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
  (* (/ a (gcd a b)) b))

(define (prime? n)
  (if (= n 1)
      #f
      (let loop ([i 2])
        (if (> i (sqrt n))
            #t
            (if (= (remainder n i) 0)
                #f
                (loop (+ i 1)))))))

(define (reverse lst)
  (if (null? lst)
      '()
      (cons (last lst) (reverse (butlast lst)))))

(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (first lst)) (map f (rest lst)))))

(define (filter f lst)
  (if (null? lst)
      '()
      (let ([rest (filter f (rest lst))])
        (if (f (first lst))
            (cons (first lst) rest)
            rest))))

(define (quicksort lst)
  (if (null? lst)
      '()
      (let ([pivot (first lst)]
            [left (filter (lambda (x) (< x pivot)) (rest lst))]
            [right (filter (lambda (x) (>= x pivot)) (rest lst))])
        (append (quicksort left) (cons pivot) (quicksort right)))))

(define (mergesort lst)
  (if (null? lst)
      '()
      (let ([mid (quotient (length lst) 2)]
            [left (take mid lst)]
            [right (drop mid lst)])
        (append (mergesort left) (mergesort right)))))

(define (binary-search lst x)
  (define (loop [low high]
    (if (> low high)
        -1
        (let ([mid (quotient (+ low high) 2)])
          (if (= (list-ref lst mid) x)
              mid
              (if (< (list-ref lst mid) x)
                  (loop (+ mid 1) high)
                  (loop low (- mid 1))))))))
  (loop 0 (- (length lst) 1)))
```

This is a collection of common algorithms and data structures implemented in Scheme. It includes functions for calculating factorials, Fibonacci numbers, greatest common divisors, least common multiples, checking primality, reversing lists, mapping functions over lists, filtering lists, sorting lists using quicksort and mergesort, and searching for elements in a list using binary search.

The code is well-commented and should be easy to understand, even if you are not familiar with Scheme. Here are some brief explanations of each function:

* `factorial`: Calculates the factorial of a number.
* `fibonacci`: Calculates the nth Fibonacci number.
* `gcd`: Calculates the greatest common divisor of two numbers.
* `lcm`: Calculates the least common multiple of two numbers.
* `prime?`: Checks if a number is prime.
* `reverse`: Reverses a list.
* `map`: Applies a function to each element of a list and returns a new list.
* `filter`: Filters a list based on a predicate and returns a new list.
* `quicksort`: Sorts a list using the quicksort algorithm.
* `mergesort`: Sorts a list using the mergesort algorithm.
* `binary-search`: Searches for an element in a sorted list using binary search.

I hope this code is useful to you!