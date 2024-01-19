```scheme
(define (matriz-cuadrada n)
  (let loop ((i 0) (matriz '()))
    (if (= i n) matriz
        (cons (make-string n #\.)
              (loop (add1 i) matriz)))))

(define (imprimir-matriz matriz)
  (for-each (fila matriz)
    (display (string-join fila ""))))

(define (rotar-matriz matriz)
  (let* ((n (length matriz))
         (matriz-rotada (matriz-cuadrada n)))
    (for-each ([(i . fila)] (enumerate matriz))
      (for-each (j (iota n))
        (set-char! (vector-ref matriz-rotada j i) (vector-ref fila (- n 1 j))))))
    matriz-rotada))

(define (main)
  (let* ((n (read))
         (matriz (apply make-matrix (repeat n (read)))))
    (for-each (i (iota n))
      (imprimir-matriz (rotar-matriz matriz)))))

(main)
```

Explicación:

* La función `matriz-cuadrada` crea una matriz cuadrada de tamaño `n` llena de puntos.
* La función `imprimir-matriz` imprime una matriz en la consola.
* La función `rotar-matriz` rota una matriz 90 grados en sentido horario.
* La función `main` lee el tamaño de la matriz y la propia matriz de la entrada estándar, y luego imprime la matriz rotada 90 grados en sentido horario.