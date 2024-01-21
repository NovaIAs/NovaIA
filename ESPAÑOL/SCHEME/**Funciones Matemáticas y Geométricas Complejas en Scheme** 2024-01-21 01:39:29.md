```scheme
(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))

(define suma-cuadrados
  (lambda (lista)
    (if (null? lista)
        0
        (+ (square (car lista))
           (suma-cuadrados (cdr lista))))))

(define combinaciones
  (lambda (n k)
    (if (= k 0)
        1
        (* (/ 1 (factorial k))
           (factorial (- n (- k 1)))))))

(define permutations
  (lambda (n k)
    (* (combinaciones n k)
       (factorial k))))

(define pascal-triangle
  (lambda (n)
    (if (= n 0)
        (list 1)
        (append (cons 1 (pascal-triangle (- n 1)))
               (map cons (cdr (pascal-triangle (- n 1)))
                  (reverse (cdr (pascal-triangle (- n 1)))))))))

(define fibonacci
  (lambda (n)
    (if (or (= n 0) (= n 1))
        n
        (+ (fibonacci (- n 1))
           (fibonacci (- n 2))))))

(define distancia-euclidiana
  (lambda (p1 p2)
    (sqrt (+ (expt (- (car p1) (car p2)) 2)
             (expt (- (cdr p1) (cdr p2)) 2))))))

(define punto-medio
  (lambda (p1 p2)
    (cons (/ (+ (car p1) (car p2)) 2)
           (/ (+ (cdr p1) (cdr p2)) 2))))

(define recta-interseccion
  (lambda (p1 p2 p3 p4)
    (let loop ((p1 p1) (p2 p2) (p3 p3) (p4 p4) (interseccion #f))
        (cond ((and (= (car p1) (car p2)) (= (cdr p1) (cdr p2)))
                #f)
              ((and (= (car p3) (car p4)) (= (cdr p3) (cdr p4)))
                #f)
              ((= (expt (- (car p1) (car p2)) (* (cdr p1) (cdr p2)))
                   (expt (- (car p3) (car p4)) (* (cdr p3) (cdr p4))))
                (loop p1 p2 p3 p4 (punto-medio @p1 @p2)))
              (else
                (cond ((= (car p1) (car p3))
                        (loop (cdr p1) (cdr p2) p3 p4 (punto-medio @p1 @p2)))
                      ((= (car p1) (car p4))
                        (loop (cdr p1) (cdr p2) p4 p3 (punto-medio @p1 @p2)))
                      ((= (car p2) (car p3))
                        (loop p1 (cdr p2) p3 p4 (punto-medio @p1 @p2)))
                      ((= (car p2) (car p4))
                        (loop p1 (cdr p2) p4 p3 (punto-medio @p1 @p2)))
                      ((= (cdr p1) (cdr p3))
                        (loop (car p1) (car p2) p3 p4 (punto-medio @p1 @p2)))
                      ((= (cdr p1) (cdr p4))
                        (loop (car p1) (car p2) p4 p3 (punto-medio @p1 @p2)))
                      ((= (cdr p2) (cdr p3))
                        (loop p1 (car p2) p3 p4 (punto-medio @p1 @p2)))
                      ((= (cdr p2) (cdr p4))
                        (loop p1 (car p2) p4 p3 (punto-medio @p1 @p2))))))))))

```

Este código incluye una variedad de funciones matemáticas y geométricas complejas en Scheme. Aquí hay una breve explicación de cada función:

* `factorial`: Calcula el factorial de un número.
* `suma-cuadrados`: Calcula la suma de los cuadrados de los elementos de una lista.
* `combinaciones`: Calcula el número de combinaciones posibles de n elementos tomados de k en k.
* `permutations`: Calcula el número de permutaciones posibles de n elementos tomados de k en k.
* `pascal-triangle`: Genera el triángulo de Pascal hasta la fila n.
* `fibonacci`: Calcula el n-ésimo número de Fibonacci.
* `distancia-euclidiana`: Calcula la distancia euclidiana entre dos puntos en un espacio bidimensional.
* `punto-medio`: Calcula el punto medio entre dos puntos en un espacio bidimensional.
* `recta-interseccion`: Calcula el punto de intersección de dos segmentos de recta en un espacio bidimensional.