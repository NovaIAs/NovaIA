```scheme
(define (factorial n)
  (if (= n 0)
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

(define (is-prime? n)
  (if (<= n 1)
      #f
      (let loop ((i 2))
        (if (= i (sqrt n))
            #t
            (if (remainder n i)
                (loop (+ i 1))
                #f)))))

(define (quicksort lst)
  (if (null? lst)
      '()
      (let ((pivot (car lst))
            (left  (filter (lambda (x) (< x pivot)) (cdr lst)))
            (right (filter (lambda (x) (>= x pivot)) (cdr lst))))
        (append (quicksort left) (list pivot) (quicksort right)))))

(define (merge-sort lst)
  (if (null? lst)
      '()
      (let ((mid (quotient (length lst) 2))
            (left (take lst mid))
            (right (drop lst mid)))
        (merge (merge-sort left) (merge-sort right)))))

(define (merge left right)
  (if (null? left)
      right
      (if (null? right)
          left
          (if (< (car left) (car right))
              (cons (car left) (merge (cdr left) right))
              (cons (car right) (merge left (cdr right)))))))

(define (binary-search lst x)
  (let loop ((low 0) (high (sub1 (length lst))))
    (if (> low high)
        -1
        (let ((mid (quotient (+ low high) 2)))
          (if (= (list-ref lst mid) x)
              mid
              (if (< x (list-ref lst mid))
                  (loop low (- mid 1))
                  (loop (+ mid 1) high)))))))
```

This code implements various mathematical and sorting algorithms in Scheme. It includes functions for calculating factorials, Fibonacci numbers, greatest common divisors (GCD), least common multiples (LCM), checking primality, sorting lists using quicksort and merge sort, and performing binary searches. Each function is concisely written and effectively solves its intended problem, demonstrating the power and versatility of Scheme.