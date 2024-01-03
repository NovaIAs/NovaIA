Certainly! Here's a complex and differentiated Scheme code that generates a random maze using the Recursive Backtracking algorithm:

```scheme
(define (generate-maze width height)
  (define (create-grid width height)
    (make-vector height (make-vector width #f)))
  
  (define (valid-cell? cell grid)
    (and (>= (car cell) 0) (>= (cdr cell) 0)
         (< (car cell) width) (< (cdr cell) height)
         (not (vector-ref (vector-ref grid (cdr cell)) (car cell)))))
  
  (define (neighbors cell grid)
    (filter (lambda (n)
              (valid-cell? n grid))
            `((,(car cell) . ,(add1 (cdr cell)))
              (,(sub1 (car cell)) . ,(cdr cell)))
              (,(car cell) . ,(sub1 (cdr cell)))
              (,(add1 (car cell)) . ,(cdr cell)))))
  
  (define (remove-wall a b grid)
    (let ((dx (- (car b) (car a)))
          (dy (- (cdr b) (cdr a))))
      (when (= dx 1)
        (vector-set! (vector-ref grid (cdr a)) (car a) #t))
      (when (= dx -1)
        (vector-set! (vector-ref grid (cdr b)) (car b) #t))
      (when (= dy 1)
        (vector-set! (vector-ref grid (cdr a)) (car a) #t))
      (when (= dy -1)
        (vector-set! (vector-ref grid (cdr b)) (car b) #t))))
  
  (define (generate-path cell grid)
    (vector-set! (vector-ref grid (cdr cell)) (car cell) #t)
    (let loop ((current cell) (stack '()))
      (let* ((neighbors (neighbors current grid))
             (unvisited (filter (lambda (n)
                                  (not (vector-ref (vector-ref grid (cdr n)) (car n)))))
                                neighbors))
        (if (null? unvisited)
            (if (null? stack)
                grid
                (loop (car stack) (cdr stack)))
            (let* ((next (list-ref unvisited (random (length unvisited))))
                   (x (car next))
                   (y (cdr next)))
              (remove-wall current next grid)
              (vector-set! (vector-ref grid y) x #t)
              (loop next (cons current stack)))))))
  
  (let* ((grid (create-grid width height))
         (start '(0 . 0))
         (maze (generate-path start grid)))
    maze))
```

This code defines a function called `generate-maze` that takes `width` and `height` as arguments and returns a randomly generated maze as a 2D grid using the Recursive Backtracking algorithm.

Explanation:
1. The `create-grid` function creates a 2D grid of cells with the given `width` and `height`.
2. The `valid-cell?` function checks if a cell is within the grid boundaries and is unvisited.
3. The `neighbors` function returns a list of neighboring cells that are within the grid boundaries and unvisited.
4. The `remove-wall` function removes the wall between two cells by updating the grid.
5. The `generate-path` function generates a random path through the maze using the Recursive Backtracking algorithm.
6. The `generate-maze` function initializes the maze grid, sets the start cell as `(0, 0)`, and generates the maze using `generate-path`.

You can call the `generate-maze` function with desired `width` and `height` parameters to get a randomly generated maze in Scheme.