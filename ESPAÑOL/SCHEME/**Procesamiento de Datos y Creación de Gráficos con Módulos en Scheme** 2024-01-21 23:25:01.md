```scheme
;; Módulo para calcular operaciones matemáticas básicas
(define-module operaciones-matemáticas
  (export sumar restar multiplicar dividir)
  (define (sumar a b) (+ a b))
  (define (restar a b) (- a b))
  (define (multiplicar a b) (* a b))
  (define (dividir a b) (/ a b))
)

;; Módulo para leer y escribir datos de un archivo
(define-module io
  (export leer-archivo escribir-archivo)
  (define (leer-archivo nombre-archivo)
    (with-input-from-file nombre-archivo
      (lambda (stream) (do ((linea (read-line stream))
                          (lineas (cons linea lineas))
                          (resto-del-archivo stream))
                        ((null? resto-del-archivo) (reverse lineas))))))
  (define (escribir-archivo nombre-archivo datos)
    (with-output-to-file nombre-archivo
      (lambda (stream) (with-current-output-port stream
                        (display datos))
                      stream)))
)

;; Módulo para procesar datos de una lista
(define-module procesamiento-de-listas
  (export promedio máximo mínimo es-impar)
  (define (promedio lista)
    (/ (sumar (apply + lista)) (length lista)))
  (define (máximo lista)
    (foldr max 0 lista))
  (define (mínimo lista)
    (foldr min (car lista) (cdr lista)))
  (define (es-impar número)
    (= (modulo número 2) 1))
)

;; Módulo para crear gráficos
(define-module gráficos
  (export crear-gráfico)
  (define (crear-gráfico datos)
    (with-current-output (current-output)
      (set-current-output (open-output-file "gráfico.txt"))
      (write-list datos current-output)
      (close-output-port current-output)))
)

;; Programa principal
(begin
  (define datos (leer-archivo "datos.txt"))
  (define promedios (map (lambda (lista) (promedio lista)) datos))
  (define maximos (map (lambda (lista) (máximo lista)) datos))
  (define minimos (map (lambda (lista) (mínimo lista)) datos))
  (define impares (filter es-impar datos))
  (crear-gráfico (list promedios maximos minimos impares))
)
```

Este código es un programa complejo en Scheme que realiza una serie de operaciones matemáticas y de procesamiento de datos, y luego crea un gráfico con los resultados. El programa consta de varios módulos, cada uno de los cuales exporta un conjunto de funciones.

El módulo `operaciones-matemáticas` define las funciones básicas de suma, resta, multiplicación y división.

El módulo `io` define las funciones para leer y escribir datos de un archivo.

El módulo `procesamiento-de-listas` define las funciones para calcular el promedio, el máximo y el mínimo de una lista, y para filtrar los elementos impares de una lista.

El módulo `gráficos` define la función para crear un gráfico con los datos proporcionados.

El programa principal lee los datos de un archivo, los procesa utilizando las funciones de los otros módulos y luego crea un gráfico con los resultados.