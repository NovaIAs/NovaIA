```scheme
(define (generate-maze width height)
  (let loop ((maze (make-array width height #f)))
    (if (and (<= 2 width) (<= 2 height))
        maze
        (let ((split-width (quotient width 2))
              (split-height (quotient height 2))
              (left-maze (loop (make-array split-width split-height #f)))
              (right-maze (loop (make-array (- width split-width) split-height #f)))
              (top-maze (loop (make-array split-width (- height split-height) #f)))
              (bottom-maze (loop (make-array (- width split-width) (- height split-height) #f))))
          (set! (aref maze split-width split-height) #t)
          (set-car! (cdr (aref maze split-width split-height)) #t)
          (set-cdr! (cdr (aref maze split-width split-height)) #t)
          (set! (aref maze 0 split-height) #t)
          (set-car! (cdr (aref maze 0 split-height)) #t)
          (set! (aref maze 0 0) #t)
          (set-cdr! (cdr (aref maze 0 0)) #t)
          (set! (aref maze split-width 0) #t)
          (set-car! (cdr (aref maze split-width 0)) #t)
          (merge-mazes maze left-maze right-maze top-maze bottom-maze)
          (loop maze))))))

(define (merge-mazes maze left right top bottom)
  (for-each (lambda (row)
              (for-each (lambda (col)
                          (if (or (and (< row 1) (< col 1))
                                  (and (< row 1) (< (- width col) 1))
                                  (and (< (- height row) 1) (< col 1))
                                  (and (< (- height row) 1) (< (- width col) 1)))
                              #t
                              (aref maze row col))))
                      row)
              left)
            (in-range (- width 1)))
  (for-each (lambda (row)
              (for-each (lambda (col)
                          (if (or (and (< row 1) (< col 1))
                                  (and (< row 1) (< (- width col) 1))
                                  (and (< (- height row) 1) (< col 1))
                                  (and (< (- height row) 1) (< (- width col) 1)))
                              #t
                              (aref maze row col))))
                      row)
              right)
            (in-range width))
  (for-each (lambda (row)
              (for-each (lambda (col)
                          (if (or (and (< row 1) (< col 1))
                                  (and (< row 1) (< (- width col) 1))
                                  (and (< (- height row) 1) (< col 1))
                                  (and (< (- height row) 1) (< (- width col) 1)))
                              #t
                              (aref maze row col))))
                      row)
              top)
            (in-range height))
  (for-each (lambda (row)
              (for-each (lambda (col)
                          (if (or (and (< row 1) (< col 1))
                                  (and (< row 1) (< (- width col) 1))
                                  (and (< (- height row) 1) (< col 1))
                                  (and (< (- height row) 1) (< (- width col) 1)))
                              #t
                              (aref maze row col))))
                      row)
              bottom)
            (in-range (- height 1))))

(define (in-range n)
  (for/list ((i 0))
    (< i n)))

(define maze (generate-maze 31 21))
(display maze)
```

This code generates a random maze using a recursive algorithm. The `generate-maze` function takes two arguments, the width and height of the maze, and returns a two-dimensional array of booleans representing the maze. The boolean values indicate whether or not a given cell is part of a wall.

The `generate-maze` function works by recursively dividing the maze into four quadrants, and then generating a maze in each quadrant. The quadrants are then merged together to form the final maze.

The `merge-mazes` function takes five arguments: the main maze, and the four quadrant mazes. It merges the four quadrant mazes into the main maze, taking care to ensure that the walls between the quadrants are removed.

The `in-range` function is a helper function that generates a list of numbers from 0 to n-1.

The `display` function prints the maze to the console.

When you run this code, it will generate a random maze and then print it to the console. The maze will be 31 cells wide and 21 cells high.