```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (es-primo n)
  (define (primo-aux n divisor)
    (if (> divisor (sqrt n))
        #t
        (if (= (mod n divisor) 0)
            #f
            (primo-aux n (+ divisor 2)))))
  (if (= n 2)
      #t
      (primo-aux n 3)))

(define (mcd a b)
  (if (= b 0)
      a
      (mcd b (mod a b))))

(define (mcm a b)
  (/ (* a b) (mcd a b)))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define (suma-divisores n)
  (define (suma-divisores-aux n divisor suma)
    (if (> divisor (sqrt n))
        suma
        (if (= (mod n divisor) 0)
            (suma-divisores-aux n (+ divisor 1) (+ divisor suma))
            (suma-divisores-aux n (+ divisor 1) suma))))
  (suma-divisores-aux n 1 0))

(define (invertir-cadena cadena)
  (if (null? cadena)
      ""
      (cons (car cadena) (invertir-cadena (cdr cadena)))))

(define (ordenar-lista lista)
  (define (ordenar-lista-aux lista ordenada)
    (if (null? lista)
        ordenada
        (ordenar-lista-aux (cdr lista) (insertar (car lista) ordenada))))
  (define (insertar elemento lista-ordenada)
    (if (null? lista-ordenada)
        (list elemento)
        (if (< elemento (car lista-ordenada))
            (cons elemento lista-ordenada)
            (cons (car lista-ordenada)
                  (insertar elemento (cdr lista-ordenada))))))
  (ordenar-lista-aux lista '()))

(define (digitos n)
  (if (= n 0)
      0
      (+ 1 (digitos (/ n 10)))))

(define (suma-digitos n)
  (if (= n 0)
      0
      (+ (mod n 10) (suma-digitos (/ n 10)))))

(define (es-palindromo cadena)
  (define (es-palindromo-aux cadena inicio fin)
    (if (> inicio fin)
        #t
        (if (= (string-ref cadena inicio) (string-ref cadena fin))
            (es-palindromo-aux cadena (+ inicio 1) (- fin 1))
            #f)))
  (es-palindromo-aux cadena 0 (- (string-length cadena) 1)))

(define (es-armstrong n)
  (define (es-armstrong-aux n suma potencia)
    (if (= n 0)
        (= suma potencia)
        (es-armstrong-aux (/ n 10) (+ suma (expt (mod n 10) potencia)) potencia)))
  (define (potencia n)
    (if (= n 0)
        1
        (* n (potencia (- n 1))))
  (es-armstrong-aux n 0 (potencia (digitos n)))))

(define (hallar-raices-cuadraticas a b c)
  (define (discriminante a b c)
    (- (* b b) (* 4 a c)))
  (if (> (discriminante a b c) 0)
      (list (/ (- b (sqrt (discriminante a b c))) (* 2 a)) (/ (+ b (sqrt (discriminante a b c))) (* 2 a)))
      '()))

(define (hallar-raices-cubicas a b c d)
  (define (cardano-aux a b c d)
    (define (p q)
      (- (/ 1 3) (* a a)))
    (define (q)
      (- (/ 2) (* b b b)))
    (define (r)
      (- (/ 27) (* a a a)))
    (define (s)
      (+ (* b c) (* (- c) (/ d))))
    (define (t)
      (+ (* 2) (* b b b) (* (- 3) (* a c))))
    (define (u)
      (- (* 3) (* a b c)))
    (define (v)
      (+ (* 2) (* b b b) (* (- 3) (* a c))))
    (define (w)
      (* 2 b))
    (define (x)
      (/ (* (- p) (* (- q) (/ r u))) (* 2)))
    (define (y)
      (/ (* (+ q) (/ r u)) (* 2)))
    (define (z)
      (/ (/ (- s) (+ t (/ w 3))) u)))
    (list (+ x z) (+ (- x) (+ (- y) z)) (- x (- y) z))))
  (cardano-aux a b c d))
```

Este código es un conjunto de funciones útiles escritas en el lenguaje de programación Scheme. Las funciones cubren una amplia gama de tareas, desde matemáticas básicas hasta procesamiento de cadenas y resolución de ecuaciones.

Aquí hay una explicación de cada función:

* `factorial`: Calcula el factorial de un número entero.
* `es-primo`: Determina si un número entero es primo.
* `mcd`: Calcula el máximo común divisor de dos números enteros.
* `mcm`: Calcula el mínimo común múltiplo de dos números enteros.
* `fibonacci`: Calcula el n-ésimo número de Fibonacci.
* `suma-divisores`: Calcula la suma de los divisores positivos de un número entero.
* `invertir-cadena`: Invierte el orden de los caracteres en una cadena.
* `ordenar-lista`: Ordena una lista de números en orden ascendente.
* `digitos`: Cuenta el número de dígitos en un número entero.
* `suma-digitos`: Calcula la suma de los dígitos de un número entero.
* `es-palindromo`: Determina si una cadena es un palíndromo.
* `es-armstrong`: Determina si un número entero es un número de Armstrong.
* `hallar-raices-cuadraticas`: Encuentra las raíces reales de una ecuación cuadrática.
* `hallar-raices-cubicas`: Encuentra las raíces reales de una ecuación cúbica.

Estas funciones son sólo una pequeña muestra de las muchas funciones útiles que se pueden escribir en Scheme. El lenguaje es muy potente y expresivo, lo que lo hace ideal para una amplia gama de tareas de programación.