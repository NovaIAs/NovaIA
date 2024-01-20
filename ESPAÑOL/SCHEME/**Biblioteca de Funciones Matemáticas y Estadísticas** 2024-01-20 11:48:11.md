```scheme
(define (buscar-elemento lista elemento)
  (cond ((null? lista) #f)
        ((equal? (car lista) elemento) lista)
        (else (buscar-elemento (cdr lista) elemento))))

(define (remover-elemento lista elemento)
  (cond ((null? lista) '())
        ((equal? (car lista) elemento) (cdr lista))
        (else (cons (car lista) (remover-elemento (cdr lista) elemento)))))

(define (insertar-elemento lista elemento)
  (cond ((null? lista) '((elemento)))
        ((> (car lista) elemento) (cons elemento lista))
        (else (cons (car lista) (insertar-elemento (cdr lista) elemento)))))

(define (ordenar-lista lista)
  (cond ((null? lista) '())
        ((null? (cdr lista)) lista)
        (else (insertar-elemento (ordenar-lista (remover-elemento lista (car lista))) (car lista)))))

(define (invertir-lista lista)
  (cond ((null? lista) '())
        (else (append (invertir-lista (cdr lista)) (list (car lista))))))

(define (palindromo? cadena)
  (equal? cadena (invertir-lista cadena)))

(define (calcular-factorial número)
  (cond ((= número 0) 1)
        (else (* número (calcular-factorial (- número 1))))))

(define (calcular-potencia base exponente)
  (cond ((= exponente 0) 1)
        (else (* base (calcular-potencia base (- exponente 1))))))

(define (calcular-raíz-cuadrada número)
  (sqrt número))

(define (calcular-área-triángulo base altura)
  (* base altura 0.5))

(define (calcular-área-círculo radio)
  (* pi (* radio radio))))

(define (calcular-volumen-esfera radio)
  (* (4/3) pi (* radio radio radio))))

(define (calcular-promedio lista)
  (/ (sum lista) (length lista)))

(define (calcular-mediana lista)
  (let ((ordenada (ordenar-lista lista)))
    (cond ((odd? (length ordenada)) (car (cdr ordenada)))
          (else (/ (+ (car ordenada) (cadr ordenada)) 2)))))

(define (calcular-moda lista)
  (let ((frecuencias (for/list ((elemento lista)) (cons elemento (count lista elemento)))))
    (max-value (map car frecuencias) (lambda (x y) (> x y)))))

(define (calcular-desviación-estándar lista)
  (let ((promedio (calcular-promedio lista)))
    (sqrt (/ (sum (map (lambda (x) (** (- x promedio) 2)) lista)) (length lista))))))

(define (calcular-coeficiente-correlación lista1 lista2)
  (let ((promedio1 (calcular-promedio lista1))
        (promedio2 (calcular-promedio lista2)))
    (/ (sum (map (lambda (x y) (* (- x promedio1) (- y promedio2))) lista1 lista2))
       (* (calcular-desviación-estándar lista1) (calcular-desviación-estándar lista2))))))
```

Este código contiene una serie de funciones útiles para realizar cálculos matemáticos y estadísticos.

* `buscar-elemento`: Esta función busca un elemento en una lista y devuelve la lista si el elemento se encuentra, o `#f` si el elemento no se encuentra.
* `remover-elemento`: Esta función elimina un elemento de una lista y devuelve la lista resultante.
* `insertar-elemento`: Esta función inserta un elemento en una lista ordenada, manteniendo el orden de la lista.
* `ordenar-lista`: Esta función ordena una lista de números en orden ascendente.
* `invertir-lista`: Esta función invierte una lista.
* `palindromo?`: Esta función comprueba si una cadena es un palíndromo, es decir, si se lee igual de izquierda a derecha que de derecha a izquierda.
* `calcular-factorial`: Esta función calcula el factorial de un número.
* `calcular-potencia`: Esta función calcula la potencia de un número.
* `calcular-raíz-cuadrada`: Esta función calcula la raíz cuadrada de un número.
* `calcular-área-triángulo`: Esta función calcula el área de un triángulo.
* `calcular-área-círculo`: Esta función calcula el área de un círculo.
* `calcular-volumen-esfera`: Esta función calcula el volumen de una esfera.
* `calcular-promedio`: Esta función calcula el promedio de una lista de números.
* `calcular-mediana`: Esta función calcula la mediana de una lista de números.
* `calcular-moda`: Esta función calcula la moda de una lista de números.
* `calcular-desviación-estándar`: Esta función calcula la desviación estándar de una lista de números.
* `calcular-coeficiente-correlación`: Esta función calcula el coeficiente de correlación entre dos listas de números.

Todas estas funciones se pueden utilizar para realizar diversos cálculos matemáticos y estadísticos.