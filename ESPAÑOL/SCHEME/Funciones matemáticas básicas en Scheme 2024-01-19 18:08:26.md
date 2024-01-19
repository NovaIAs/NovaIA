```scheme
;; Definir una función para calcular el promedio de una lista de números

(define (promedio lista)
  (if (null? lista)
      0
      (/ (suma lista) (largo lista))))

;; Definir una función para calcular la suma de una lista de números

(define (suma lista)
  (if (null? lista)
      0
      (+ (primero lista) (suma (resto lista)))))

;; Definir una función para calcular el largo de una lista

(define (largo lista)
  (if (null? lista)
      0
      (+ 1 (largo (resto lista)))))

;; Definir una función para encontrar el máximo de una lista de números

(define (maximo lista)
  (define (maximo-aux max lista)
    (if (null? lista)
        max
        (maximo-aux (max (primero lista) max) (resto lista))))
  (maximo-aux (primero lista) (resto lista)))

;; Definir una función para encontrar el mínimo de una lista de números

(define (minimo lista)
  (define (minimo-aux min lista)
    (if (null? lista)
        min
        (minimo-aux (min (primero lista) min) (resto lista))))
  (minimo-aux (primero lista) (resto lista)))

;; Definir una función para encontrar la mediana de una lista de números

(define (mediana lista)
  (let ((ordenada (ordenar lista)))
    (if (par? (largo ordenada))
        (/ (+ (elemento-en ordenada (/ (largo ordenada) 2))
               (elemento-en ordenada (/ (- (largo ordenada) 1) 2)))
           2)
        (elemento-en ordenada (/ (largo ordenada) 2)))))

;; Definir una función para ordenar una lista de números

(define (ordenar lista)
  (if (null? lista)
      '()
      (insertar (primero lista) (ordenar (resto lista)))))

;; Definir una función para insertar un elemento en una lista ordenada

(define (insertar elemento lista)
  (if (null? lista)
      '(elemento)
      (if (< elemento (primero lista))
          (cons elemento lista)
          (cons (primero lista) (insertar elemento (resto lista))))))

;; Definir una función para encontrar un elemento en una lista

(define (elemento-en lista posicion)
  (if (= posicion 0)
      (primero lista)
      (elemento-en (resto lista) (- posicion 1))))

;; Definir una función para determinar si una lista es par o impar

(define (par? lista)
  (= 0 (modulo (largo lista) 2)))
```