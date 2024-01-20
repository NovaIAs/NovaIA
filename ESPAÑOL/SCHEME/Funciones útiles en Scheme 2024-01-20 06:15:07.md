```scheme
(define (factorial n)
  (cond
    [(zero? n) 1]
    [else (* n (factorial (- n 1)))]))

(define (fibonacci n)
  (define (fib-iter i prev curr)
    (if (= i n)
      curr
      (fib-iter (+ i 1) curr (+ prev curr))))
  (fib-iter 2 0 1))

(define (es-primo n)
  (define (primo? divisor)
    (zero? (modulo n divisor)))
  (cond
    [(= n 1) false]
    [(= n 2) true]
    [(even? n) false]
    [else (and (primo? 3)
              (primo? 5)
              (primo? 7)
              (primo? 11)
              (primo? 13)
              (primo? 17)
              (primo? 19))]))

(define (suma-lista list)
  (if (null? list)
    0
    (+ (car list) (suma-lista (cdr list)))))

(define (concat-listas list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (concat-listas (cdr list1) list2))))

(define (max-lista list)
  (define (max-iter max-actual list)
    (if (null? list)
      max-actual
      (if (> (car list) max-actual)
        (max-iter (car list) (cdr list))
        (max-iter max-actual (cdr list)))))
  (max-iter (car list) (cdr list)))

(define (min-lista list)
  (define (min-iter min-actual list)
    (if (null? list)
      min-actual
      (if (< (car list) min-actual)
        (min-iter (car list) (cdr list))
        (min-iter min-actual (cdr list)))))
  (min-iter (car list) (cdr list)))

(define (ordenar-lista list)
  (define (insertar elem lista)
    (if (null? lista)
      (list elem)
      (if (> elem (car lista))
        (cons (car lista) (insertar elem (cdr lista)))
        (cons elem lista))))
  (define (ordenar-lista-iter lista result)
    (if (null? lista)
      result
      (ordenar-lista-iter (cdr lista) (insertar (car lista) result))))
  (ordenar-lista-iter (cdr list) (cons (car list) '())))

(define (invertir-lista list)
  (define (invertir-lista-iter lista result)
    (if (null? lista)
      result
      (invertir-lista-iter (cdr lista) (cons (car lista) result))))
  (invertir-lista-iter (cdr list) (cons (car list) '())))

(define (encontrar-elemento elemento lista)
  (if (null? lista)
    false
    (if (= elemento (car lista))
      true
      (encontrar-elemento elemento (cdr lista)))))

(define (eliminar-elemento elemento lista)
  (if (null? lista)
    '()
    (if (= elemento (car lista))
      (cdr lista)
      (cons (car lista) (eliminar-elemento elemento (cdr lista))))))

(define (longitud-lista lista)
  (define (longitud-lista-iter lista contador)
    (if (null? lista)
      contador
      (longitud-lista-iter (cdr lista) (+ contador 1))))
  (longitud-lista-iter (cdr list) 1))

(define (promedio-lista lista)
  (/ (suma-lista lista) (longitud-lista lista)))

(define (mediana-lista lista)
  (ordenar-lista lista)
  (let ((mitad (quotient (longitud-lista lista) 2)))
    (if (even? (longitud-lista lista))
      (/ (+ (car (nth mitad lista)) (car (nth (+ mitad 1) lista))) 2)
      (car (nth mitad lista)))))

(define (moda-lista lista)
  (define (moda-iter moda-actual frecuencia-actual lista)
    (if (null? lista)
      (cons moda-actual frecuencia-actual)
      (let ((frecuencia (count lista (car lista))))
        (if (> frecuencia frecuencia-actual)
          (moda-iter (car lista) frecuencia (cdr lista))
          (moda-iter moda-actual frecuencia-actual (cdr lista))))))
  (define (moda-lista-iter lista moda-actual frecuencia-actual)
    (if (null? lista)
      (moda-actual)
      (let ((frecuencia (count lista (car lista))))
        (if (> frecuencia frecuencia-actual)
          (moda-lista-iter (cdr lista) (car lista) frecuencia)
          (moda-lista-iter (cdr lista) moda-actual frecuencia-actual)))))
  (let* ((moda-frecuencia (moda-iter '() 0 lista))
         (moda (car moda-frecuencia))
         (frecuencia (cdr moda-frecuencia)))
    (moda-lista-iter (cdr lista) moda frecuencia)))
```

Este código implementa una variedad de funciones útiles en Scheme, que incluyen:

* `factorial`: Calcula el factorial de un número positivo.
* `fibonacci`: Calcula el $n$-ésimo número de Fibonacci.
* `es-primo`: Comprueba si un número es primo.
* `suma-lista`: Calcula la suma de los elementos de una lista.
* `concat-listas`: Concatena dos listas en una sola lista.
* `max-lista`: Encuentra el elemento máximo de una lista.
* `min-lista`: Encuentra el elemento mínimo de una lista.
* `ordenar-lista`: Ordena una lista de números de menor a mayor.
* `invertir-lista`: Invierte el orden de los elementos de una lista.
* `encontrar-elemento`: Comprueba si un elemento se encuentra en una lista.
* `eliminar-elemento`: Elimina un elemento de una lista.
* `longitud-lista`: Calcula la longitud de una lista.
* `promedio-lista`: Calcula el promedio de los elementos de una lista.
* `mediana-lista`: Calcula la mediana de los elementos de una lista.
* `moda-lista`: Calcula la moda de los elementos de una lista.