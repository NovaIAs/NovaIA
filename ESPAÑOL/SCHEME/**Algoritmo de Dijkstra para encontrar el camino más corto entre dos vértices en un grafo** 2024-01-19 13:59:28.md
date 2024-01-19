```scheme
(define-syntax for-each-pair
  (syntax-rules ()
    ((for-each-pair (var1 var2) seq forms ...)
     (lambda (env)
       (let/rec loop ((seq seq))
         (if (null? seq)
             '()
             (let ((cdr (cdr seq)))
               `((,var1 (car seq)) (,var2 (car cdr)) ,@forms)
                  (loop cdr)))))) )

(define-syntax let-pair
  (syntax-rules ()
    ((let-pair ((var1 var2) val seq forms ...)
     (lambda (env)
       (let/rec loop ((seq seq))
         (if (null? seq)
             '()
             (let ((cdr (cdr seq)))
               `((begin (set! ,var1 (car seq)) (set! ,var2 (car cdr))) ,@forms)
                  (loop cdr)))))) )

(define (zip-with f l1 l2)
  (for-each-pair ((x y) l1 l2)
    (cons (f x y) '())))

(define (make-table size initial)
  (let-values (((s (make-vector size)
                 (fill! (vector-fill! s initial)))))
    s))

(define (pos-in-vector v elem)
  (vector-ref (vector->list v) elem))

(define (shortest-path-lengths v from)
  (let ((size (vector-length v)
        table (make-table size '()))
    (pos-in-vector! table from 0)
    (define (visit-edge! v w new_dist)
      (let ((old-dist (pos-in-vector table w)))
        (when (and old-dist (> new_dist old-dist))
              (pos-in-vector! table w new_dist))))
    (define (visit-edges! edges d)
      (for-each (edge edges)
        (let ((w (edge-weight edge)
              new_dist (+ d w)))
          (visit-edge! v (edge-destination edge) new_dist))))
    (define (bfs)
      (let ((q '((from . 0))
            marked '())
        (while (not (null? q))
          (let ((v (car q)))
            (if (member v marked)
                (q (cdr q))
                (begin
                  (visit-edges! (edge-list v) (vector-ref table v))
                  (vector-set! marked (vector->list marked) v 1)
                  (q (append q (edge-map car (edge-list v))))))))))
    (bfs)
    table))

(define (show-shortest-path t from goal)
  (let loop ((g goal
              p (pos-in-vector t goal)))
    (if (zero? g)
        '(ok)
        (let ((v (vector-ref (edge-table p) 0))
              (e (edge-by-destination v goal)))
          (display-line (edge-source e))
          (display-line (edge-destination e))
          (loop v (edge-source e))))))

(define (shortest-path v from goal)
  (let ((t (shortest-path-lengths v from)))
    (cond ((null? t) '())
          ((zero? (pos-in-vector t goal)) '())
          (else (show-shortest-path t from goal)))))
```

Este código implementa el algoritmo de Dijkstra para encontrar el camino más corto entre dos vértices en un grafo.

El código comienza definiendo varias sintaxis y macros para hacer el código más conciso y fácil de leer.

La función `zip-with` toma una función y dos listas y devuelve una lista de los resultados de aplicar la función a los elementos correspondientes de las listas.

La función `make-table` crea una tabla de valores vacíos de un tamaño determinado.

La función `pos-in-vector` devuelve la posición de un elemento en un vector.

La función `shortest-path-lengths` toma un grafo y un vértice de origen y devuelve una tabla de las distancias más cortas desde el vértice de origen a todos los demás vértices del grafo.

La función `show-shortest-path` toma una tabla de distancias más cortas y un vértice de destino y muestra el camino más corto desde el vértice de origen al vértice de destino.

La función `shortest-path` toma un grafo, un vértice de origen y un vértice de destino y devuelve el camino más corto desde el vértice de origen al vértice de destino.