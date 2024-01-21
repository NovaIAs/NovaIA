```scheme
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* a b) (/ a (gcd a b)))

(define (prime? n)
  (if (<= n 1)
      #f
      (for/list ((i (range 2 (sqrt n))))
        (if (= (remainder n i) 0)
            #f
            #t))))

(define (divisors n)
  (for/list ((i (range 1 (+ n 1))))
    (if (= (remainder n i) 0)
        i)))

(define (perfect-number? n)
  (let ((sum 0))
    (for/each ((i (divisors n)))
      (set! sum (+ sum i))))
  (= sum n))

(define (goldbach-conjecture n)
  (if (< n 4)
      #f
      (for/list ((i (range 2 (/ n 2))))
        (if (and (prime? i) (prime? (- n i)))
            (list i (- n i))
            () ))))

(define (hanoi-tower n)
  (for/list ((i (range 1 (+ n 1))))
    (displayln (format "Mover el disco %d de la torre A a la torre C" i))))

(define (n-queens n)
  (for/list ((queens (permutations (range 1 (+ n 1)))))
    (if (every? (lambda (q1 q2) (not (= q1 q2) (not (= (abs (- q1 q2)) n)))) queens)
        queens
        ())))

(define (sudoku-solver board)
  (if (every? (lambda (row) (every? (lambda (cell) (not (= cell 0))) row)) board)
      #t
      (for/list ((i (range (length board))))
        (for/list ((j (range (length board))))
          (if (= (board i j) 0)
              (for/each ((value (range 1 (+ board 1))))
                (if (every? (lambda (row)
                              (not (= value (board row j)))) board)
                    (if (every? (lambda (col)
                                  (not (= value (board i col)))) board)
                        (if (every? (lambda (cell)
                                      (not (= value cell)))
                                  (sudoku-solver (set-value! board i j value)))
                            (sudoku-solver board))
                        board)
                    board)
                board))))))
```

Este código contiene una colección de funciones matemáticas y algorítmicas complejas en Scheme:

* **factorial:** Calcula el factorial de un número.
* **fibonacci:** Calcula el número de Fibonacci de un índice.
* **gcd:** Calcula el máximo común divisor de dos números.
* **lcm:** Calcula el mínimo común múltiplo de dos números.
* **prime?:** Determina si un número es primo.
* **divisors:** Calcula los divisores de un número.
* **perfect-number?:** Determina si un número es un número perfecto.
* **goldbach-conjecture:** Comprueba la conjetura de Goldbach para un número par.
* **hanoi-tower:** Imprime las instrucciones para mover las torres de Hanoi.
* **n-queens:** Calcula todas las soluciones del problema de las n reinas.
* **sudoku-solver:** Resuelve un rompecabezas de Sudoku.

Estas funciones son bastante complejas, pero también son muy útiles para resolver una variedad de problemas matemáticos y algorítmicos.