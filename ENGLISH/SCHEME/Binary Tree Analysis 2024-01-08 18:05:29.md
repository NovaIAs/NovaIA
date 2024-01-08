```scheme
(define make-tree
  (lambda (n)
    (if (zero? n)
        '()
        (list (make-tree (sub1 n))
              (make-tree (sub1 n))
              n))))

(define print-tree
  (lambda (tree)
    (if (null? tree)
        '()
        (display (car tree))
        (newline)
        (print-tree (cadr tree))
        (print-tree (caddr tree)))))

(define count-leaves
  (lambda (tree)
    (if (null? tree)
        0
        (+ (count-leaves (cadr tree))
           (count-leaves (caddr tree))
           (if (null? (cadr tree))
               1
               0)))))

(define sum-leaves
  (lambda (tree)
    (if (null? tree)
        0
        (+ (sum-leaves (cadr tree))
           (sum-leaves (caddr tree))
           (if (null? (cadr tree))
               (car tree)
               0)))))

(define max-depth
  (lambda (tree)
    (if (null? tree)
        0
        (max (+ 1 (max-depth (cadr tree)))
             (+ 1 (max-depth (caddr tree)))))))

(define balance?
  (lambda (tree)
    (if (null? tree)
        'true
        (and (balance? (cadr tree))
             (balance? (caddr tree))
             (= (max-depth (cadr tree))
                (max-depth (caddr tree)))))))

(define print-leaves
  (lambda (tree)
    (if (null? tree)
        '()
        (if (null? (cadr tree))
            (display (car tree))
            (print-leaves (cadr tree)))
        (if (null? (caddr tree))
            (display (car tree))
            (print-leaves (caddr tree))))))

(define (main)
  (let loop ((n 10))
    (if (<= n 0)
        '()
        (begin
          (display "Tree with " n " nodes:")
          (newline)
          (print-tree (make-tree n))
          (newline)
          (display "Number of leaves: " (count-leaves (make-tree n)))
          (newline)
          (display "Sum of leaves: " (sum-leaves (make-tree n)))
          (newline)
          (display "Maximum depth: " (max-depth (make-tree n)))
          (newline)
          (if (balance? (make-tree n))
              (display "Tree is balanced")
              (display "Tree is not balanced"))
          (newline)
          (display "Leaves: ")
          (print-leaves (make-tree n))
          (newline)
          (newline)
          (loop (sub1 n))))))

(main)
```

Explanation:

The code first defines a function `make-tree` which takes a number `n` as input and returns a binary tree with `n` nodes. The function `print-tree` takes a tree as input and prints it to the console.

The code then defines a function `count-leaves` which takes a tree as input and returns the number of leaves in the tree. The function `sum-leaves` takes a tree as input and returns the sum of the values of the leaves in the tree.

The code then defines a function `max-depth` which takes a tree as input and returns the maximum depth of the tree. The function `balance?` takes a tree as input and returns `true` if the tree is balanced, and `false` otherwise.

The code then defines a function `print-leaves` which takes a tree as input and prints the values of the leaves in the tree to the console.

The code then defines a function `main` which calls the `make-tree`, `print-tree`, `count-leaves`, `sum-leaves`, `max-depth`, `balance?`, and `print-leaves` functions for a tree with 10 nodes.

The code then calls the `main` function to print the tree, the number of leaves, the sum of the leaves, the maximum depth, whether the tree is balanced, and the values of the leaves.