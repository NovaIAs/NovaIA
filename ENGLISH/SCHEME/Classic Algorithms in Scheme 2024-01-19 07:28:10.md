```scheme
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (prime? n)
  (if (or (<= n 1) (= n 2))
      #t
      (and (not (even? n))
           (for/and ([i 3 (+ i 2)])
             (< i (sqrt n))
             (not (= (remainder n i) 0))))))

(define (fibonacci n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (queens n)
  (define (safe? col row queens)
    (for*/and ([i 0 (- row 1)])
      (not (or (= col (queens i)) (= (- col i) (- row i)) (= (+ col i) (+ row i))))))

  (define (place-queens row queens)
    (cond ((= row n)
           (cons queens '()))
          ((for/any ([col 0 (- n 1)])
                (safe? col row queens))
           (place-queens (+ row 1) (cons col queens)))))

  (place-queens 0 '()))

(define (hanoi n)
  (define (move n from to via)
    (display (list 'move n from to via)))

  (define (solve n from to via)
    (if (= n 1)
        (move 1 from to via)
        (begin
          (solve (- n 1) from via to)
          (move n from to via)
          (solve (- n 1) via to from))))

  (solve n 'A 'B 'C))

(define (quicksort xs)
  (if (empty? xs)
      '()
      (let ((pivot (car xs))
            (lesser (filter (lambda (x) (< x pivot)) (cdr xs)))
            (greater (filter (lambda (x) (>= x pivot)) (cdr xs))))
        (append (quicksort lesser) pivot (quicksort greater)))))

(define (mergesort xs)
  (if (empty? xs)
      '()
      (let* ((n (/ (length xs) 2))
             (left (mergesort (take n xs)))
             (right (mergesort (drop n xs))))
        (merge left right))))
```

This code implements several classic algorithms in Scheme, including factorial, greatest common divisor, least common multiple, primality testing, Fibonacci numbers, the 8-queens puzzle, the Towers of Hanoi, quicksort, and mergesort.

Here are some explanations for each algorithm:

* **Factorial:** The factorial of a non-negative integer `n` is the product of all positive integers less than or equal to `n`. The recursive function `factorial` calculates the factorial of `n` by multiplying `n` by the factorial of `n-1`.

* **Greatest Common Divisor (GCD):** The greatest common divisor of two integers `a` and `b` is the largest positive integer that divides both `a` and `b` without leaving a remainder. The function `gcd` calculates the GCD of `a` and `b` using Euclid's algorithm.

* **Least Common Multiple (LCM):** The least common multiple of two integers `a` and `b` is the smallest positive integer that is divisible by both `a` and `b`. The function `lcm` calculates the LCM of `a` and `b` by multiplying `a` and `b`, and then dividing by their GCD.

* **Primality Testing:** Primality testing determines whether a given integer `n` is prime or not. A prime number is a natural number greater than 1 that is not a product of two smaller natural numbers. The function `prime?` checks if `n` is prime using a combination of divisibility checks and the primality test.

* **Fibonacci Numbers:** Fibonacci numbers are a series of numbers in which each number is the sum of the two preceding numbers. The function `fibonacci` calculates the `n`th Fibonacci number using a recursive approach.

* **8-Queens Puzzle:** The 8-queens puzzle is a classic puzzle in which the goal is to place eight queens on a chessboard so that no two queens threaten each other. The function `queens` generates all possible solutions to the 8-queens puzzle using a backtracking algorithm.

* **Towers of Hanoi:** The Towers of Hanoi is a mathematical game or puzzle. It consists of three rods and a number of disks of different sizes, which can slide onto any rod. The puzzle starts with the disks in a neat stack in ascending order of size on one rod, the smallest at the top, thus making a conical shape. The objective of the puzzle is to move the entire stack to another rod, obeying the following simple rules:
 1. Only one disk can be moved at a time.
 2. Each move consists of taking the upper disk from one of the stacks and placing it on top of another stack i.e. a disk can only be moved if it is the uppermost disk on a stack.
 3. No disk may be placed on top of a smaller disk.

The function `hanoi` solves the Towers of Hanoi puzzle for `n` disks using a recursive algorithm.

* **Quicksort:** Quicksort is a sorting algorithm that uses a divide-and-conquer approach. It works by selecting a pivot element, partitioning the array into two sub-arrays based on the pivot, and recursively sorting each sub-array. The function `quicksort` implements the quicksort algorithm.

* **Mergesort:** Mergesort is a sorting algorithm that uses a divide-and-conquer approach. It works by recursively dividing the array into smaller sub-arrays, sorting each sub-array, and then merging the sorted sub-arrays back together. The function `mergesort` implements the mergesort algorithm.