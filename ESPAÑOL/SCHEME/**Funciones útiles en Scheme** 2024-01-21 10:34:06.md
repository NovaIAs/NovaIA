```scheme

(define-syntax `cuando (cond ([condición (exp1 exp2 ...)]) ...)

(define (factorial n)
    (si (= n 0)
        1
        (* n (factorial (- n 1)))))

(define (suma-cuadrados n)
    (si (= n 0)
        0
        (+ (* n n) (suma-cuadrados (- n 1)))))

(define (fibonacci n)
    (si (= n 0)
        0
        (si (= n 1)
            1
            (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define (es-primo n)
    (si (= 2 n)
        verdadero
        (si (cero? (modulo n 2))
            falso
            (define variante 3)
            (while (< variante (sqrt n))
                (si (cero? (modulo n variante))
                    falso
                    (define variante (+ variante 2))))))))

(define (mayor-elemento lista)
    (if (vacía? lista)
        (error "Lista vacía")
        (if (tipo-de-dato (car lista) 'número)
            (car lista)
            (mayor-elemento (cdr lista)))))

(define (insertar-elemento elemento lista)
    (cond ((vacía? lista)
           '(elemento))
          ((< (car lista) elemento)
           (cons (car lista) (insertar-elemento elemento (cdr lista))))
          (else
           (cons elemento lista))))

(define (ordenar-lista lista)
    (cond ((vacía? lista)
           '(nil))
          ((tipo-de-dato (car lista) 'número)
           (insertar-elemento (car lista) (ordenar-lista (cdr lista))))
          (else
           (error "Elementos de la lista no son números")))))

(define (string-reverse str)
    (if (vacía? str)
        ""
        (cons (último-elemento str) (string-reverse (sin-último-elemento str)))))

(define (invertir-lista lista)
    (if (vacía? lista)
        '(nil))
    (cons (último-elemento lista) (invertir-lista (sin-último-elemento lista)))))

(define (es-palíndromo str)
    (= str (string-reverse str)))

(define (suma-arrays array1 array2)
    (cond ((vacía? array1)
           array2)
          ((vacía? array2)
           array1)
          (else
           (cons (+ (primer-elemento array1) (primer-elemento array2)) (suma-arrays (sin-primer-elemento array1) (sin-primer-elemento array2))))))

(define (producto-punto vector1 vector2)
    (cond ((vacía? vector1)
           0)
          ((vacía? vector2)
           0)
          (else
           (+ (* (primer-elemento vector1) (primer-elemento vector2)) (producto-punto (sin-primer-elemento vector1) (sin-primer-elemento vector2))))))

(define (encontrar-elemento elemento lista)
    (cond ((vacía? lista)
           falso)
          ((= elemento (car lista))
           verdadero)
          (else
           (encontrar-elemento elemento (cdr lista)))))

(define (eliminar-elemento elemento lista)
    (cond ((vacía? lista)
           '(nil))
          ((= elemento (car lista))
           (cdr lista))
          (else
           (cons (car lista) (eliminar-elemento elemento (cdr lista))))))

(define (unir-listas lista1 lista2)
    (cond ((vacía? lista1)
           lista2)
          ((vacía? lista2)
           lista1)
          (else
           (cons (car lista1) (unir-listas (cdr lista1) lista2))))))

(define (aplanar-lista lista)
    (cond ((vacía? lista)
           '(nil))
          ((tipo-de-dato (car lista) 'lista)
           (unir-listas (aplanar-lista (car lista)) (aplanar-lista (cdr lista))))
          (else
           (cons (car lista) (aplanar-lista (cdr lista))))))

(define (buscar-máximo lista)
    (cond ((vacía? lista)
           (error "Lista vacía"))
          ((tipo-de-dato (car lista) 'número)
           (mayor-elemento lista))
          (else
           (error "Elementos de la lista no son números")))))

(define (buscar-mínimo lista)
    (cond ((vacía? lista)
           (error "Lista vacía"))
          ((tipo-de-dato (car lista) 'número)
           (define min (car lista))
           (define resto (cdr lista))
           (mientras (> resto '())
               (si (> (car resto) min)
                   (define min (car resto))
                   (define resto (cdr resto)))
           min)
          (else
           (error "Elementos de la lista no son números")))))

```

Este código es un conjunto de funciones útiles en Scheme, que incluyen:

* `cuando`: Una macro condicional que permite escribir condiciones en un formato más natural.
* `factorial`: Calcula el factorial de un número.
* `suma-cuadrados`: Calcula la suma de los cuadrados de los números de 1 a n.
* `fibonacci`: Calcula el n-ésimo número de Fibonacci.
* `es-primo`: Comprueba si un número es primo.
* `mayor-elemento`: Devuelve el mayor elemento de una lista de números.
* `insertar-elemento`: Inserta un elemento en una lista ordenada.
* `ordenar-lista`: Ordena una lista de números.
* `string-reverse`: Invierte una cadena de caracteres.
* `invertir-lista`: Invierte una lista.
* `es-palíndromo`: Comprueba si una cadena de caracteres es un palíndromo.
* `suma-arrays`: Suma dos arrays de números.
* `producto-punto`: Calcula el producto punto de dos vectores.
* `encontrar-elemento`: Busca un elemento en una lista.
* `eliminar-elemento`: Elimina un elemento de una lista.
* `unir-listas`: Une dos listas.
* `aplanar-lista`: Aplana una lista de listas en una sola lista.
* `buscar-máximo`: Busca el máximo elemento de una lista de números.
* `buscar-mínimo`: Busca el mínimo elemento de una lista de números.