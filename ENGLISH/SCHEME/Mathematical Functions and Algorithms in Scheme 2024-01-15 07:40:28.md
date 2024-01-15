```scheme
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (is-prime n)
  (cond ((= n 1) #f)
        ((= n 2) #t)
        ((even? n) #f)
        (else (for/any ([i (in-range 3 (sqrt n) 2)])
                    (not (= (modulo n i) 0))))))

(define (sieve-of-eratosthenes n)
  (define (mark-multiples-of-p a p)
    (for ([i (in-range (* p p) n (+ p p))])
        (set! (a i) #f)))
  (define a (make-vector n #t))
  (set! (a 1) #f)
  (for ([i (in-range 2 (sqrt n))])
      (when (vector-ref a i)
        (mark-multiples-of-p a i)))
  (vector->list a))

(define (goldbach-conjecture n)
  (if (even? n)
      (for*/list ([a (in-range 2 (/ n 2))])
                  ([b (in-range a (/ n 2))])
                  (= (+ a b) n))
      '()))

(define (fibonacci n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define (hanoi n)
  (if (zero? n)
      '()
      (append (hanoi (- n 1))
              (list (list 'A 'B))
              (hanoi (- n 1)))))

(define (queens n)
  (define (is-safe row col)
    (for/any* ([i (in-range 0 row)])
              ([j (in-range 0 col)])
              (or (= row i)
                  (= col j)
                  (= (+ row (- col i)) (+ n (- row j))))))
  (for/list ([sol (in-range 0 (expt 2 n))])
      (let loop ((row 0) (sol sol) (result '()))
        (if (zero? row)
            result
            (let ([safe-cols (for/list ([col (in-range 0 n)])
                                (is-safe row col))])
              (if (null? safe-cols)
                  result
                  (loop (+ row 1)
                        (quotient sol 2)
                        (cons (1+ (modulo sol (length safe-cols)))
                              result)))))))))
```

This code implements a variety of mathematical functions and algorithms in Scheme. Here's a brief explanation of each function:

* `factorial`: Calculates the factorial of a non-negative integer `n`, which is the product of all positive integers from 1 to `n`.

* `is-prime`: Checks if a given integer `n` is a prime number, meaning it has exactly two divisors: 1 and itself.

* `sieve-of-eratosthenes`: Generates a list of prime numbers up to a given integer `n` using the Sieve of Eratosthenes algorithm.

* `goldbach-conjecture`: Tests the Goldbach conjecture, which states that every even integer greater than 2 can be expressed as the sum of two primes.

* `fibonacci`: Calculates the nth Fibonacci number, which is the sum of the two preceding Fibonacci numbers.

* `hanoi`: Generates a sequence of moves to solve the Tower of Hanoi puzzle for a given number of disks `n`.

* `queens`: Finds all solutions to the N-queens problem, which is the task of placing `n` queens on an `n x n` chessboard such that no two queens threaten each other.