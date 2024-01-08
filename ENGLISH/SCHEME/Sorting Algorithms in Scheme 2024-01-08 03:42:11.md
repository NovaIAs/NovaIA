```scheme

(define (factorial n)
  (if (= n 1) 1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (or (< n 0) (= n 1)) n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (prime? n)
  (if (or (<= n 1) (= n 2)) true
      (every? (lambda (i) (not (zero? (remainder n i))))
              (range 2 (ceiling (sqrt n))))))

(define (combinations n k)
  (if (= k 0) 1
      (* (- n 1) (combinations (- n 1) (- k 1)))))

(define (permutations n)
  (factorial n))

(define (subsets n)
  (expt 2 n))

(define (power-set s)
  (if (null? s) (list '())
      (let ((x (car s))
            (xs (cdr s)))
        (append (map (lambda (ys) (cons x ys)) (power-set xs))
                (power-set xs))))))

(define (partition n l)
  (let loop ((i 0) (acc '()))
    (if (= i n) acc
        (loop (+ i 1) (append acc (list (take i l)))))))

(define (zip l1 l2)
  (if (or (null? l1) (null? l2)) '()
      (cons (list (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(define (unzip l)
  (if (null? l) (values '() '())
      (let ((h (car l))
            (t (cdr l)))
        (let (([x y] (unzip t)))
          (values (cons (car h) x) (cons (cdr h) y))))))

(define (merge l1 l2)
  (let loop ((l1 l1) (l2 l2) (acc '()))
    (if (and (null? l1) (null? l2)) acc
        (if (or (null? l1) (> (car l1) (car l2)))
            (loop (cdr l1) l2 (cons (car l1) acc))
            (loop l1 (cdr l2) (cons (car l2) acc))))))

(define (quicksort l)
  (if (null? l) '()
      (let ((pivot (car l))
            (less (filter (lambda (x) (< x pivot)) (cdr l)))
            (greater (filter (lambda (x) (>= x pivot)) (cdr l))))
        (append (quicksort less) (list pivot) (quicksort greater)))))

(define (heapsort l)
  (define (max-heapify h n)
    (define (heapify i)
      (define (left (* 2 i))
            (right (+ (* 2 i) 1))
            (largest i)
            (l (if (< left n) (h left) -1))
            (r (if (< right n) (h right) -1)))
      (if (and (< i n) (> (h i) l) (> (h i) r))
          (let ((temp (h i)))
            (set! (h i) (max l r))
            (set! (h (heapify largest)) temp))
          i)))
    (let loop ((i (- n 1)))
      (if (< i 0) h
          (loop (heapify i)))))
  (define (build-max-heap l) (max-heapify l (length l)))
  (let ((h (build-max-heap l))
        (n (length h)))
    (for/list ((i (in-list (range 1 n))))
      (let ((temp (car h)))
        (set! (car h) (h n))
        (set! (h n) temp)
        (max-heapify h (- i 1))))
      h)))

(define (mergesort l)
  (if (null? l) '()
      (let ((mid (quotient (length l) 2))
            (left (mergesort (take mid l)))
            (right (mergesort (drop mid l))))
        (merge left right))))

(define (insertion-sort l)
  (let loop ((sorted '()) (unsorted l))
    (if (null? unsorted) sorted
        (loop (cons (car unsorted) sorted) (cdr unsorted)))))

(define (selection-sort l)
  (let loop ((sorted '()) (unsorted l))
    (if (null? unsorted) sorted
        (loop (cons (min unsorted) sorted)
              (remove (min unsorted) unsorted)))))

(define (bubble-sort l)
  (let loop ((l l) (swapped? #f))
    (if (null? l) '()
        (let ((x (car l))
              (xs (cdr l)))
          (if (and (not (null? xs)) (> x (car xs)))
              (loop (cons (car xs) (cons x (cdr xs))) #t)
              (loop xs swapped?)))))))

```

This code implements a variety of sorting algorithms in Scheme. The algorithms included are:

* **Factorial:** Calculates the factorial of a number.
* **Fibonacci:** Calculates the nth Fibonacci number.
* **GCD:** Calculates the greatest common divisor of two numbers.
* **LCM:** Calculates the least common multiple of two numbers.
* **Prime?:** Checks if a number is prime.
* **Combinations:** Calculates the number of ways to choose k elements from a set of n elements.
* **Permutations:** Calculates the number of ways to arrange n elements in a specific order.
* **Subsets:** Calculates the number of subsets of a set of n elements.
* **Power Set:** Calculates the power set of a set.
* **Partition:** Divides a list into n sublists of equal length.
* **Zip:** Zips together two lists into a list of pairs.
* **Unzip:** Unzips a list of pairs into two lists.
* **Merge:** Merges two sorted lists into a single sorted list.
* **Quicksort:** Sorts a list using the quicksort algorithm.
* **Heapsort:** Sorts a list using the heapsort algorithm.
* **Mergesort:** Sorts a list using the mergesort algorithm.
* **Insertion Sort:** Sorts a list using the insertion sort algorithm.
* **Selection Sort:** Sorts a list using the selection sort algorithm.
* **Bubble Sort:** Sorts a list using the bubble sort algorithm.