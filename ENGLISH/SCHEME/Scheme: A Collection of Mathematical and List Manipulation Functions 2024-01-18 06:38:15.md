```scheme
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (or (zero? n) (zero? (- n 1)))
      1
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (is-prime n)
  (if (or (zero? n) (negative? n))
      #f
      (define (prime? p)
        (if (zero? (modulo n p))
            #f
            (if (> (* p p) n)
                #t
                (prime? (+ p 1)))))
      (prime? 2)))

(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (modulo a b))))

(define (lcm a b)
  (* a b) (/ a (gcd a b))))

(define (count-occurrences lst elem)
  (if (null? lst)
      0
      (+ (if (= elem (car lst)) 1 0)
         (count-occurrences (cdr lst) elem))))

(define (remove-element lst elem)
  (if (null? lst)
      '()
      (cons (car lst) (remove-element (cdr lst) elem))))

(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (cons (car lst) (remove-duplicates (remove-element (cdr lst) (car lst))))))

(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst)) (list (car lst)))))

(define (quicksort lst)
  (if (null? lst)
      '()
      (let ((pivot (car lst)))
        (append (quicksort (filter (lambda (x) (< x pivot)) (cdr lst)))
                (list pivot)
                (quicksort (filter (lambda (x) (>= x pivot)) (cdr lst)))))))

(define (mergesort lst)
  (if (null? lst)
      '()
      (let* ((mid (quotient (length lst) 2))
             (left (take mid lst))
             (right (drop mid lst)))
        (append (mergesort left) (mergesort right)))))
```

This Scheme code provides a variety of useful mathematical and list manipulation functions:

1. `factorial`: Calculates the factorial of a non-negative integer.

2. `fibonacci`: Calculates the nth Fibonacci number.

3. `is-prime`: Checks if a number is prime.

4. `gcd`: Calculates the greatest common divisor of two numbers.

5. `lcm`: Calculates the least common multiple of two numbers.

6. `count-occurrences`: Counts the number of occurrences of a given element in a list.

7. `remove-element`: Removes all occurrences of a given element from a list.

8. `remove-duplicates`: Removes duplicate elements from a list.

9. `reverse`: Reverses a list.

10. `quicksort`: Sorts a list using the quicksort algorithm.

11. `mergesort`: Sorts a list using the merge sort algorithm.

These functions demonstrate various programming concepts like recursion, filtering, and sorting algorithms. They provide useful tools for mathematical calculations and list processing in Scheme.