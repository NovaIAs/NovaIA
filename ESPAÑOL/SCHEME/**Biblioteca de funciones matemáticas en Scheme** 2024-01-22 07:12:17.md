```scheme
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

(define (combinaciones n k)
  (if (or (zero? n) (zero? k) (>= n k))
      1
      (* (factorial n) (/ 1 (factorial (- n k)) (factorial k)))))

(define (pascal_triangle n)
  (let loop ((result '(())) (i 0))
    (if (> i n)
        result
        (let ((row (make-list (+ i 1) '0)))
          (set! (car row) 1)
          (set! (cadr row) 1)
          (dotimes (j 1 (- (length row) 2))
            (set! (list-ref row (+ j 1)) (+ (list-ref row j) (list-ref row (+ j 1)))))
          (cons row (loop result (+ i 1)))))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (primo? n)
  (if (or (zero? n) (negative? n))
      #f
      (let loop ((i 2))
        (if (or (>= i n) (= n 1))
            #t
            (if (= (remainder n i) 0)
                #f
                (loop (+ i 1)))))))

(define (factorizar n)
  (let loop ((factor 2) (result '()))
    (if (or (> factor (/ (sqrt n) 2)) (= n 1))
        result
        (if (= (remainder n factor) 0)
            (cons factor (loop (+ factor 1) (cons factor result)))
            (loop (+ factor 1) result)))))

(define (mcd a b)
  (if (zero? a)
      b
      (if (zero? b)
          a
          (mcd b (remainder a b)))))

(define (mcm a b)
  (* (/ a (mcd a b)) b))

(define (potencia a n)
  (if (zero? n)
      1
      (* a (potencia a (- n 1)))))

(define (raiz_cuadrada n)
  (sqrt n))

(define (logaritmo_natural n)
  (log n))

(define (logaritmo_base_10 n)
  (log10 n))
```

Este código incluye una variedad de funciones matemáticas útiles, que van desde funciones básicas como factoriales y combinaciones hasta funciones más complejas como el cálculo de la serie de Fibonacci o la factorización de números.

Aquí hay una explicación de cada función:

* `factorial`: Calcula el factorial de un número.
* `combinaciones`: Calcula el número de combinaciones posibles de un conjunto de elementos.
* `pascal_triangle`: Genera el triángulo de Pascal.
* `fibonacci`: Calcula el n-ésimo número de Fibonacci.
* `gcd`: Calcula el máximo común divisor de dos números.
* `lcm`: Calcula el mínimo común múltiplo de dos números.
* `primo?`: Determina si un número es primo.
* `factorizar`: Factoriza un número en sus factores primos.
* `mcd`: Calcula el máximo común divisor de dos números.
* `mcm`: Calcula el mínimo común múltiplo de dos números.
* `potencia`: Calcula la potencia de un número a una potencia dada.
* `raiz_cuadrada`: Calcula la raíz cuadrada de un número.
* `logaritmo_natural`: Calcula el logaritmo natural de un número.
* `logaritmo_base_10`: Calcula el logaritmo en base 10 de un número.