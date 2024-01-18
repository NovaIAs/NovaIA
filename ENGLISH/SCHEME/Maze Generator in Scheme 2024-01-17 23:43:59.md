```scheme
(define (make-maze w h)
  (let loop ((x 0) (y 0) (maze (make-matrix w h #f)))
    (if (and (>= x w) (>= y h))
      maze
      (set-matrix! maze x y #t)
      (random-walk (if (zero? x) (+ w 1) x) (if (zero? y) (+ h 1) y) maze)
      (loop (1- x) (1+ y) maze)))))

(define (random-walk x y maze)
  (let loop ()
    (if (and (>= x 0) (< x (matrix-width maze))
           (>= y 0) (< y (matrix-height maze))
           (matrix-ref maze x y))
      (loop)
      (let ((dir (random 4)))
        (cond ((= dir 0) (set-matrix! maze (- x 1) y #t) (loop (1- x) y)))
              ((= dir 1) (set-matrix! maze (1+ x) y #t) (loop (1+ x) y)))
              ((= dir 2) (set-matrix! maze x (- y 1) #t) (loop x (1- y))))
              ((= dir 3) (set-matrix! maze x (1+ y) #t) (loop x (1+ y))))))))))

(define w 40)
(define h 20)
(define maze (make-maze w h))

(define (display-maze maze)
  (for-each (lambda (row)
              (newline)
              (display (vector->string (map (lambda (cell) (if cell "#" " ")) row))))
            (matrix->list maze)))

(display-maze maze)
```

This Scheme code is a function that generates a random maze using a depth-first search algorithm.

The function `make-maze` takes two arguments: the width and height of the maze to be generated. It returns a matrix representing the maze, where each element is either `#` (a wall) or `.` (a path).

The function `random-walk` is a recursive function that performs a depth-first search of the maze, starting from a given point. It randomly chooses a direction to move in, and then recursively calls itself to explore the maze in that direction. If it reaches a dead end, it backtracks and tries a different direction.

The function `display-maze` is used to display the maze to the console. It takes a matrix representing the maze as an argument, and prints it out using the `newline` and `display` functions.

To use this code, you can call the `make-maze` function with the desired width and height of the maze, and then call the `display-maze` function to display the maze to the console. For example, the following code will generate a 40x20 maze and display it to the console:

```scheme
(define w 40)
(define h 20)
(define maze (make-maze w h))
(display-maze maze)
```