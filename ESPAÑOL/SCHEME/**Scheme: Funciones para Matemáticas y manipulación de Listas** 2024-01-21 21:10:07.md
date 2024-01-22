```scheme
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (suma-factorial n)
  (if (= n 1)
      1
      (+ (factorial n) (suma-factorial (- n 1)))))

(define (mayor-menor lista)
  (let ((mayor (list-ref lista 0))
        (menor (list-ref lista 0)))
    (do ((i 1 (1+ i)))
        ((= i (length lista)))
        (if (> (list-ref lista i) mayor)
            (set! mayor (list-ref lista i))
            (if (< (list-ref lista i) menor)
                (set! menor (list-ref lista i))
                '())))
    (values mayor menor)))

(define (ordenar-lista lista)
  (define (ordenar-lista* lista)
    (if (= (length lista) 0)
        '()
        (let ((pares (split-at (/ (length lista) 2) lista)))
            (append (ordenar-lista* (car pares)) (cons (cadr pares) (ordenar-lista* (cdr pares)))))))
  (ordenar-lista* (insertion-sort lista)))

(define (suma-lista lista)
  (if (= (length lista) 0)
      0
      (+ (car lista) (suma-lista (cdr lista)))))

(define (es-primo? n)
  (let ((i 2))
    (while (<= i (sqrt n))
      (if (zero? (modulo n i))
          #f
          (set! i (1+ i)))
      #t)))

(define (buscar-lista elemento lista)
  (if (= (length lista) 0)
      #f
      (if (= elemento (car lista))
          #t
          (buscar-lista elemento (cdr lista)))))

(define (insertar-lista elemento lista)
  (if (= (length lista) 0)
      (cons elemento '())
      (if (<= elemento (car lista))
          (cons elemento lista)
          (cons (car lista) (insertar-lista elemento (cdr lista))))))

(define (eliminar-lista elemento lista)
  (if (= (length lista) 0)
      '()
      (if (= elemento (car lista))
          (cdr lista)
          (cons (car lista) (eliminar-lista elemento (cdr lista))))))

(define (invertir-lista lista)
  (if (= (length lista) 0)
      '()
      (append (invertir-lista (cdr lista)) (cons (car lista) '()))))

(define (combinaciones lista k)
  (if (zero? k)
      '((lista)))
    (if (= (length lista) 0)
        '()
        (append (map (lambda (x) (cons x (combinaciones (cdr lista) (- k 1)))) (cons (car lista) '())) (combinaciones (cdr lista) k))))

(define (permutaciones lista)
  (if (= (length lista) 0)
      '((lista)))
    (let ((res '()))
        (do ((i 0 (< i (length lista))))
            ((= i (length lista)))
            (res (append res (map (lambda (x) (cons x (eliminar-lista x lista))) (cons (list-ref lista i) '())))))
        res))

(define (producto-escalar v1 v2)
  (suma-lista (map (lambda (x y) (* x y)) v1 v2)))

(define (diagonal-principal m)
  (map (lambda (i) (list-ref m i i)) (range (length m))))

(define (diagonal-secundaria m)
  (map (lambda (i) (list-ref m (- (length m) 1+ i) i)) (range (length m))))

(define (trasponer m)
  (map (lambda (fila) (map (lambda (col) (list-ref m col fila)) (range (length m)))) (range (length m))))

(define (determinante m)
  (if (= (length m) 2)
      (- (* (list-ref m 0 0) (list-ref m 1 1)) (* (list-ref m 0 1) (list-ref m 1 0)))
      (suma-lista (map (lambda (fila) (* (list-ref m 0 fila) (determinante (submatriz m 0 fila)))) (range (length m))))))

(define (submatriz m fila columna)
  (map (lambda (fila') (remove (lambda (x) (= x columna)) fila')) (remove (lambda (x) (= x fila)) m))))```

Explicación del código:

1. **Factorial:** Esta función calcula el factorial de un número `n`. El factorial de un número entero `n` se define como el producto de todos los enteros positivos desde `1` hasta `n`.

2. **Suma Factorial:** Esta función calcula la suma de los factoriales de todos los números enteros desde `1` hasta `n`.

3. **Mayor-Menor:** Esta función encuentra el mayor y el menor elemento de una lista.

4. **Ordenar-Lista:** Esta función ordena una lista de números en orden ascendente.

5. **Suma-Lista:** Esta función calcula la suma de todos los elementos de una lista.

6. **Es-Primo?:** Esta función comprueba si un número es primo o no. Un número primo es un número natural mayor que `1` que no es producto de dos números naturales menores.

7. **Buscar-Lista:** Esta función busca un elemento en una lista.

8. **Insertar-Lista:** Esta función inserta un elemento en una lista en orden ascendente.

9. **Eliminar-Lista:** Esta función elimina un elemento de una lista.

10. **Invertir-Lista:** Esta función invierte el orden de los elementos de una lista.

11. **Combinaciones:** Esta función genera todas las combinaciones posibles de `k` elementos de una lista.

12. **Permutaciones:** Esta función genera todas las permutaciones posibles de los elementos de una lista.

13. **Producto-Escalar:** Esta función calcula el producto escalar de dos vectores.

14. **Diagonal-Principal:** Esta función devuelve la diagonal principal de una matriz.

15. **Diagonal-Secundaria:** Esta función devuelve la diagonal secundaria de una matriz.

16. **Trasponer:** Esta función transpone una matriz.