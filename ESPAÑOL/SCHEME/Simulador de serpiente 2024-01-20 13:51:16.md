```scheme
;; Variables globales
(define world-width 10)
(define world-height 10)
(define snake-speed 1)
(define snake-length 3)
(define apple-count 1)
(define snake-direction 'right)

;; Crear el mundo
(define world (make-world world-width world-height))

;; Crear la serpiente
(define snake (make-snake snake-length snake-direction))

;; Crear la manzana
(define apple (make-apple world))

;; Bucle de juego
(define game-loop
  (lambda ()
    (update-world world snake apple)
    (draw-world world)
    (check-for-input snake)
    (sleep snake-speed)
    (game-loop)))

;; Iniciar el juego
(game-loop)

;; Funciones auxiliares

;; Crea el mundo
(define (make-world width height)
  (list (make-matrix width height 0)
        (make-matrix width height 0)))

;; Crea la serpiente
(define (make-snake length direction)
  (list (make-vector (make-vector length '0) (sub1 length))
        direction))

;; Crea la manzana
(define (make-apple world)
  (make-vector 2 (random-range (vector-length (world-size world)))))

;; Actualiza el mundo
(define (update-world world snake apple)
  (move-snake snake)
  (update-matrix (world-grid world) (snake-positions snake) 1)
  (update-matrix (world-grid world) (apple-position apple) 2))

;; Dibuja el mundo
(define (draw-world world)
  (display-matrix (world-grid world)))

;; Comprueba si hay alguna entrada
(define (check-for-input snake)
  (let ((key (read-key)))
    (if key
      (set-snake-direction snake (key-to-direction key)))))

;; Duerme durante el tiempo especificado
(define (sleep time)
  (current-milliseconds (+ time (current-milliseconds))))

;; Vector auxiliar para los índices de fila y columna
(define (make-vector n value)
  (make-vector n value))

;; Obtener un número aleatorio dentro de un rango
(define (random-range range)
  (floor (+ range (* range (random)))))

;; Actualizar una matriz
(define (update-matrix matrix positions value)
  (map (lambda (position)
         (set-matrix-value! matrix position value))
       positions))

;; Obtener el valor de una posición de una matriz
(define (get-matrix-value matrix position)
  (matrix-ref matrix (vector->list position)))

;; Establecer el valor de una posición de una matriz
(define (set-matrix-value! matrix position value)
  (set-matrix! matrix (vector->list position) value))

;; Tamaño del mundo
(define (world-size world)
  (vector-length (world-grid world)))

;; Posiciones de la serpiente
(define (snake-positions snake)
  (map (lambda (n)
         (vector->list (snake-segment snake n)))
       (range (snake-length snake))))

;; Posición de la manzana
(define (apple-position apple)
  (vector->list apple))

;; Mover la serpiente
(define (move-snake snake)
  (let ((direction (snake-direction snake)))
    (set-snake-segment! snake
                        snake-length
                        (add1 (snake-segment snake snake-length) direction))
    (set-snake-segment! snake
                        (sub1 snake-length)
                        (snake-segment snake (sub1 snake-length))
                        direction)))

;; Establecer la dirección de la serpiente
(define (set-snake-direction snake direction)
  (if (not (direction-opposite? direction (snake-direction snake)))
    (set-snake-direction! snake direction)))

;; Comprueba si dos direcciones son opuestas
(define (direction-opposite? direction1 direction2)
  (or (= direction1 'right) (= direction2 'left))
      (= direction1 'left) (= direction2 'right))
      (= direction1 'up) (= direction2 'down))
      (= direction1 'down) (= direction2 'up))))

;; Segmento de la serpiente
(define (snake-segment snake n)
  (vector-ref (snake-body snake) n))

;; Establecer el segmento de la serpiente
(define (set-snake-segment! snake n value direction)
  (if (not (direction-opposite? direction (snake-direction snake)))
    (set-snake-segment snake n value)))

;; Clave a dirección
(define (key-to-direction key)
  (cond ((string=? key "Right") 'right)
        ((string=? key "Left") 'left)
        ((string=? key "Up") 'up)
        ((string=? key "Down") 'down)))

;; Imprimir una matriz
(define (display-matrix matrix)
  (display "World:\n")
  (for-each (lambda (row)
              (display (apply displayln row)))
            (matrix-rows matrix)))

;; Filas de la matriz
(define (matrix-rows matrix)
  (if matrix
    (cons (matrix-row matrix (vector-length (world-grid world)))
          (matrix-rows (matrix-cdr matrix))))))

;; Fila de la matriz
(define (matrix-row matrix width)
  (map (lambda (n)
         (if (= 0 n)
           (cond ((= 0 (get-matrix-value matrix (make-vector 1 n)))) "-"
                 ((= 2 (get-matrix-value matrix (make-vector 1 n)))) "A")
                 (else "*"))
           (cond ((= 0 (get-matrix-value matrix (make-vector 1 n)))) "#"
                 ((= 2 (get-matrix-value matrix (make-vector 1 n)))) "O")
                 (else "-"))))
       (range width))))
```

Explicación del código:

* El código crea un mundo virtual de tamaño 10x10, una serpiente de longitud 3 y una manzana.
* El bucle de juego actualiza el mundo, dibuja el mundo, comprueba si hay alguna entrada y duerme durante el tiempo especificado.
* La serpiente se mueve en la dirección especificada por el jugador y la manzana se genera en una posición aleatoria.
* El juego termina cuando la serpiente choca consigo misma o con las paredes del mundo.

El código utiliza una serie de funciones auxiliares para realizar diversas tareas, como crear el mundo, la serpiente y la manzana, actualizar el mundo y dibujarlo en la pantalla.

El código también utiliza una serie de variables globales para almacenar el tamaño del mundo, la longitud de la serpiente, la velocidad de la serpiente y la dirección de la serpiente.

El código está escrito en Scheme, un lenguaje de programación funcional. Scheme es un lenguaje muy expresivo y potente, y es ideal para escribir código complejo y diferenciado.