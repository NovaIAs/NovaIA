```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (suma-cuadrados n)
  (if (= n 0)
      0
      (+ (expt n 2) (suma-cuadrados (- n 1)))))

(define (suma-impares n)
  (if (= n 0)
      0
      (+ n (suma-impares (- n 2)))))

(define (promedio lst)
  (/ (suma lst) (length lst)))

(define (mediana lst)
  (let ((sorted (sort lst)))
    (if (even? (length sorted))
        (average (list (car sorted) (cadr sorted)))
        (car sorted))))

(define (desviacion-estandar lst)
  (let* ((avg (promedio lst))
         (sum-squares (suma-cuadrados lst)))
    (sqrt (/ (- sum-squares (* avg avg) (* (length lst) (expt avg 2)))
           (- (length lst) 1)))))

(define (moda lst)
  (let ((cnt (count-lst lst)))
    (let ((max-cnt (max cnt)))
      (for/list ((k v) cnt)
        (when (= v max-cnt)
          k)))))

(define (count-lst lst)
  (cond ((null? lst) '())
        ((number? (car lst)) (cons (car lst) (count-lst (cdr lst))))
        ((symbol? (car lst)) (cons (car lst) (count-lst (cdr lst))))
        ((pair? (car lst)) (append (count-lst (car lst)) (count-lst (cdr lst))))
        (else (cons (list (car lst)) (count-lst (cdr lst))))))

(define (fibonacci n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (primo? x)
  (for/list (i (in-range 2 (sqrt x)))
    (when (= 0 (remainder x i))
      #f)))

(define (digitos x)
  (if (= 0 x)
      0
      (+ 1 (digitos (/ x 10)))))

(define (sumar-digitos x)
  (if (< 10 x)
      x
      (+ (modulo x 10) (sumar-digitos (/ x 10)))))

(define (invertir-digitos x)
  (if (= 0 x)
      0
      (+ (* (modulo x 10) (expt 10 (- (digitos x) 1)))
         (invertir-digitos (/ x 10)))))

(define (palindromo? x)
  (= x (invertir-digitos x)))
```

Este código es una colección de funciones útiles en Scheme, que incluyen:

* `factorial`: Calcula el factorial de un número.
* `suma-cuadrados`: Calcula la suma de los cuadrados de los números enteros de 1 a `n`.
* `suma-impares`: Calcula la suma de los números impares de 1 a `n`.
* `promedio`: Calcula el promedio de una lista de números.
* `mediana`: Calcula la mediana de una lista de números.
* `desviacion-estandar`: Calcula la desviación estándar de una lista de números.
* `moda`: Calcula la moda de una lista de números.
* `count-lst`: Cuenta el número de ocurrencias de cada elemento en una lista.
* `fibonacci`: Calcula el n-ésimo número de Fibonacci.
* `primo?`: Comprueba si un número es primo.
* `digitos`: Cuenta el número de dígitos en un número.
* `sumar-digitos`: Calcula la suma de los dígitos de un número.
* `invertir-digitos`: Invierte los dígitos de un número.
* `palindromo?`: Comprueba si un número es un palíndromo.