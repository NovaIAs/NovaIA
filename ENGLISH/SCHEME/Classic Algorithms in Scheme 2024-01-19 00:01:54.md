```scheme

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define (queens n)
  (define (queens-helper board row)
    (if (= row n)
        board
        (let loop ((col 0) (board board))
          (if (= col n)
              '()
              (let ((new-board (cons (list row col) board)))
                (if (safe? new-board)
                    (queens-helper new-board (+ row 1))
                    (loop (+ col 1) new-board)))))))

  (queens-helper '() 0))

(define (safe? board)
  (define (safe-helper queen-row queen-col board)
    (let loop ((row (- queen-row 1)))
      (if (< row 0)
          #t
          (let ((col (car (car board))))
            (if (or (= queen-col col)
                   (= queen-row (+ row (- col queen-col))))
                #f
                (loop (- row 1)))))))

  (let loop ((board board))
    (if (null? board)
        #t
        (let ((queen-row (car (car board)))
              (queen-col (cdr (car board))))
          (if (safe-helper queen-row queen-col (cdr board))
              (loop (cdr board))
              #f)))))

(define (hanoi n)
  (define (hanoi-helper n from-peg to-peg via-peg)
    (if (= n 0)
        '()
        (append (hanoi-helper (- n 1) from-peg via-peg to-peg)
                (list (list from-peg to-peg))
                (hanoi-helper (- n 1) via-peg to-peg from-peg))))

  (hanoi-helper n 1 2 3))

(define (merge-sort lst)
  (if (null? lst)
      '()
      (let ((mid (quotient (length lst) 2))
            (left (merge-sort (take lst mid)))
            (right (merge-sort (drop lst mid))))
        (merge left right))))

(define (merge left right)
  (define (merge-helper left right result)
    (if (null? left)
        (append result right)
        (if (null? right)
            (append left result)
            (let ((left-head (car left))
                  (right-head (car right)))
              (if (< left-head right-head)
                  (merge-helper (cdr left) right (cons left-head result))
                  (merge-helper left (cdr right) (cons right-head result)))))))

  (merge-helper left right '()))

(define (quick-sort lst)
  (if (null? lst)
      '()
      (let ((pivot (car lst))
            (left (filter (lambda (x) (< x pivot)) (cdr lst)))
            (right (filter (lambda (x) (>= x pivot)) (cdr lst))))
        (append (quick-sort left)
                (list pivot)
                (quick-sort right)))))

```

This is a Scheme program that implements several classic algorithms: factorial, Fibonacci, queens, Hanoi, merge sort, and quick sort.

* The `factorial` function computes the factorial of a given number.
* The `fibonacci` function computes the nth Fibonacci number.
* The `queens` function finds all solutions to the n-queens problem.
* The `hanoi` function finds all solutions to the Towers of Hanoi problem.
* The `merge-sort` function sorts a list using the merge sort algorithm.
* The `quick-sort` function sorts a list using the quick sort algorithm.

The program is organized into a series of mutually recursive functions. Each function is defined in terms of simpler functions, until the base case is reached.

The program is written in a functional style, which means that it avoids using mutable state. This makes the program easier to understand and reason about.

Here are some additional details about the program:

* The `safe?` function checks if a given board is safe for the n-queens problem.
* The `hanoi-helper` function recursively computes all solutions to the Towers of Hanoi problem.
* The `merge-helper` function recursively merges two sorted lists into a single sorted list.
* The `quick-sort` function uses the quicksort algorithm to sort a list.

The program is a good example of how to use Scheme to implement complex algorithms in a clear and concise way.