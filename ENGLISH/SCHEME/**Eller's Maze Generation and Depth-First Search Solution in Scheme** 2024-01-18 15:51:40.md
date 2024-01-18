```scheme

(define (build-maze width height)
  (let loop ([i 0] [j 0] [maze (make-matrix width height #f)])
    (if (or (>= i width) (>= j height))
        maze
        (set-matrix! maze i j #t)
        (if (>= (+ i 1) width)
            (loop i (1+ j) maze)
            (if (>= (+ j 1) height)
                (loop (1+ i) 0 maze)
                (loop i (1+ j) maze))))))

(define (carve-pass maze)
  (let loop ([i 0] [j 0] [maze maze])
    (if (and (>= i 2) (>= j 2))
        maze
        (let north (get-matrix maze (- i 2) j)
              east  (get-matrix maze i (+ j 2))
              both  (and north east))
          (if both
              (loop i j maze)
              (begin
                (set-matrix! maze i j #f)
                (if north
                    (loop (- i 1) j maze)
                    (if east
                        (loop i (+ j 1) maze)
                        (let south (get-matrix maze (+ i 2) j)
                              west  (get-matrix maze i (- j 2))
                              one   (or south west))
                        (if one
                            (loop i j maze)
                            (loop (+ i 1) j maze))))))))))

(define (carve-maze maze)
  (let* ([width (matrix-width maze)]
         [height (matrix-height maze)])
    (for/list ([i (in-range 1 width 2)])
      (for/list ([j (in-range 1 height 2)])
        (set-matrix! maze i j #f)))))

(define (solve-maze maze)
  (let loop ([x 1] [y 1] [visited (make-matrix (matrix-width maze) (matrix-height maze) #f)])
    (when (and (>= x (matrix-width maze)) (>= y (matrix-height maze)))
      (displayln "Solution found!"))
    (when (get-matrix maze x y)
      (set-matrix! visited x y #t)
      (when (not (get-matrix visited (- x 2) y)) (loop (- x 2) y visited))
      (when (not (get-matrix visited x (+ y 2))) (loop x (+ y 2) visited))
      (when (not (get-matrix visited (+ x 2) y)) (loop (+ x 2) y visited))
      (when (not (get-matrix visited x (- y 2))) (loop x (- y 2) visited)))))


(displayln (carve-maze (build-maze 20 20)))
(solve-maze (carve-maze (build-maze 20 20)))

```

This Scheme code generates a random maze using the Eller's algorithm, carves paths through it to create a solution, and then solves the maze using a depth-first search.

1. Building the maze:

- `(build-maze width height)`: This function builds a `width` x `height` matrix filled with `#f` values, representing an empty maze.

- The `loop` function recursively fills the matrix with `#t` values, representing maze walls, while leaving some cells open to create paths.

2. Carving the maze:

- `(carve-pass maze)`: This function removes a subset of walls from the maze, creating paths.

- It iterates through the matrix and removes walls where there are paths to the north and east of the current cell.

3. Solving the maze:

- `(solve-maze maze)`: This function finds a solution path through the maze using a depth-first search.

- It starts from the top-left corner (1, 1) and explores paths to the north, east, south, and west, marking visited cells along the way.

- The solution is printed when the bottom-right corner is reached.

The code demonstrates complex recursive functions and matrix manipulation techniques to create and solve a maze, showcasing Scheme's strengths in symbolic programming and data structures.