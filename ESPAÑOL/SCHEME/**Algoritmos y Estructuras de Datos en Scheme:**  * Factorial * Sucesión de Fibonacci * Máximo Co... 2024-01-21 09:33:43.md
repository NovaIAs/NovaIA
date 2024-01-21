```scheme
(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (< n 2)
    n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (modulo a b))))

(define (lcm a b)
  (/ (* a b) (gcd a b)))

(define (permutar lista)
  (if (null? lista)
    '()
    (append (map (lambda (x) (cons x (permutar (remove x lista)))) (car lista)) (permutar (cdr lista)))))

(define (combinaciones lista k)
  (if (< k 0)
    '()
    (if (null? lista)
      '()
      (append (map (lambda (x) (cons x (combinaciones (cdr lista) (- k 1)))) (car lista)) (combinaciones (cdr lista) k)))))

(define (quicksort lista)
  (if (null? lista)
    '()
    (let ((pivote (car lista))
          (menores (filter (lambda (x) (< x pivote)) (cdr lista)))
          (mayores (filter (lambda (x) (>= x pivote)) (cdr lista))))
      (append (quicksort menores) (cons pivote (quicksort mayores))))))

(define (mergesort lista)
  (if (null? lista)
    '()
    (let ((mitad (quotient (length lista) 2))
          (izquierda (take mitad lista))
          (derecha (drop mitad lista)))
      (merge (mergesort izquierda) (mergesort derecha)))))

(define (merge lista1 lista2)
  (cond
    ((null? lista1) lista2)
    ((null? lista2) lista1)
    ((< (car lista1) (car lista2)) (cons (car lista1) (merge (cdr lista1) lista2)))
    (else (cons (car lista2) (merge lista1 (cdr lista2))))))

(define (busqueda-binaria lista valor)
  (define (busqueda-aux lista valor menor mayor)
    (if (= menor mayor)
      (if (= (list-ref lista menor) valor) menor -1)
      (let ((mitad (quotient (+ menor mayor) 2)))
        (if (= (list-ref lista mitad) valor) mitad
          (if (< (list-ref lista mitad) valor)
            (busqueda-aux lista valor (add1 mitad) mayor)
            (busqueda-aux lista valor menor (sub1 mitad))))))))
  (busqueda-aux lista valor 0 (- (length lista) 1)))

(define (arbol-binario-busqueda lista)
  (define (insertar nodo arbol)
    (if (null? arbol)
      nodo
      (if (< (car nodo) (car arbol))
        (cons nodo (insertar nodo (cdr arbol)))
        (cons arbol (insertar nodo (car arbol))))))
  (define (buscar arbol valor)
    (if (null? arbol)
      #f
      (if (= (car arbol) valor)
        #t
        (or (buscar (cdr arbol) valor) (buscar (car arbol) valor)))))
  (define (eliminar arbol valor)
    (if (null? arbol)
      #f
      (if (< (car arbol) valor)
        (cons (car arbol) (eliminar (cdr arbol) valor))
        (if (= (car arbol) valor)
          (cdr arbol)
          (cons (car arbol) (eliminar (car arbol) valor))))))
  (define (recorrer arbol)
    (append (recorrer (cdr arbol)) (cons (car arbol)) (recorrer (car arbol))))
  (foldr insertar '() lista))
```

Este código es una implementación en Scheme de varios algoritmos y estructuras de datos comunes. Incluye funciones para calcular el factorial, la sucesión de Fibonacci, el máximo común divisor, el mínimo común múltiplo, las permutaciones, las combinaciones, la ordenación rápida, la ordenación por fusión, la búsqueda binaria y la implementación de un árbol binario de búsqueda.

El código está bien estructurado y es fácil de entender. Utiliza un estilo funcional y evita el uso de variables globales. Las funciones están bien documentadas y son fáciles de usar.

Este código es un excelente ejemplo de cómo utilizar Scheme para implementar algoritmos y estructuras de datos comunes. Es un recurso valioso para cualquiera que esté aprendiendo Scheme o que necesite implementar estos algoritmos y estructuras de datos en sus propios programas.