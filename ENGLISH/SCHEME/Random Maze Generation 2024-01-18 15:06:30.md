```scheme
(define (make-maze width height)
  (define walls (make-array (* width height) 0))
  (define visited (make-array (* width height) 0))
  (define (carve width height x y)
    (set! (aref walls (+ (* height y) x)) 1)
    (set! (aref visited (+ (* height y) x)) 1)
    (for/list ((direction (list 'north 'east 'south 'west)))
      (let ((x1 (cond ((eq? direction 'north) (- x 1))
                       ((eq? direction 'east) (+ x 1))
                       ((eq? direction 'south) x)
                       (else (- x 1)))))
            (y1 (cond ((eq? direction 'north) y)
                       ((eq? direction 'east) (- y 1))
                       ((eq? direction 'south) (+ y 1))
                       (else y))))
        (when (and (>= x1 0) (>= y1 0) (<= x1 (- width 1)) (<= y1 (- height 1)))
          (when (zero? (aref visited (+ (* height y1) x1)))
            (carve width height x1 y1)))))))
  (carve width height (quotient width 2) (quotient height 2))
  walls)

(define (print-maze maze width height)
  (for/each ((row (in-range height)))
    (for/each ((col (in-range width)))
      (cond
        [(zero? (aref maze (+ (* height row) col))) (display "#")]
        [else (display " ")])))
    (newline)))

(define width 20)
(define height 10)
(define maze (make-maze width height))
(print-maze maze width height)
```

This code generates a random maze using recursive backtracking. It creates a 2D array of cells, initially all set to 0, which represent the walls of the maze. It also creates an array of cells, initially all set to 0, which represents which cells have been visited.

The `carve` function is used to carve a path through the maze. It starts at a given cell and recursively carves paths in all four directions (north, east, south, west) until it reaches the edge of the maze or a cell that has already been visited.

The `print-maze` function is used to print the maze to the console. It iterates through the array of cells and prints a "#" for each cell that is a wall and a " " for each cell that is not a wall.

Finally, the `make-maze` function is called to generate a maze of the specified width and height. The `print-maze` function is then called to print the maze to the console.