```scheme
(definir funcion-fibonacci
    (lambda
        (n)
        (si (> n 1)
            (+ (funcion-fibonacci (- n 1))
                 (funcion-fibonacci (- n 2)))
            1)))

(definir funcion-factorial
    (lambda
        (n)
        (si (= n 0)
            1
            (* n (funcion-factorial (- n 1))))))

(definir funcion-quicksort
    (lambda
        (lista)
        (si (null? lista)
            '())
            (let* ((pivote (car lista))
                   (menor (filter < pivote (cdr lista)))
                   (mayor (filter >= pivote (cdr lista))))
                (append (funcion-quicksort menor)
                       (cons pivote)
                       (funcion-quicksort mayor))))))

(definir funcion-generar-arbol-binario
    (lambda
        (n)
        (si (= n 0)
            '()
            (let ((izquierda (funcion-generar-arbol-binario (- n 1)))
                  (derecha  (funcion-generar-arbol-binario (- n 1))))
                (cons (+ 1 (random 10))
                     (cons izquierda derecha))))))

(definir funcion-busqueda-binaria
    (lambda
        (lista elemento)
        (let ((izquierda 0)
                (derecha (- (length lista) 1)))
            (mientras (!= izquierda derecha)
                (let ((medio (quotient (+ izquierda derecha) 2)))
                    (si (> (lista-ref lista medio) elemento)
                        (establecer! derecha (- derecha 1)))
                    (si (< (lista-ref lista medio) elemento)
                        (establecer! izquierda (+ izquierda 1)))
                    (si (= (lista-ref lista medio) elemento)
                        (devolver medio)))
                (establecer! izquierda (+ izquierda 1))))
            -1)))

(imprimir "Fibonacci de 10: " (funcion-fibonacci 10))
(imprimir "Factorial de 5: " (funcion-factorial 5))
(imprimir "QuickSort de [3, 1, 4, 2, 5]: " (funcion-quicksort '(3 1 4 2 5)))
(imprimir "Generar árbol binario de 5: " (funcion-generar-arbol-binario 5))
(imprimir "Búsqueda binaria de 3 en [1, 2, 3, 4, 5]: " (funcion-busqueda-binaria '(1 2 3 4 5) 3))
```

Este código incluye varias funciones complejas en SCHEME, con explicaciones en español:

1. fibonacci: Calcula el n-ésimo número de Fibonacci usando recursión.

2. factorial: Calcula el factorial de un número utilizando recursión.

3. quicksort: Implementa el algoritmo de ordenación rápida para ordenar una lista.

4. generar-arbol-binario: Genera un árbol binario aleatorio de altura n.

5. busqueda-binaria: Realiza una búsqueda binaria en una lista ordenada para encontrar una elemento específico.

Espero que este código sea lo suficientemente complejo y diferenciado como para satisfacer sus requisitos.