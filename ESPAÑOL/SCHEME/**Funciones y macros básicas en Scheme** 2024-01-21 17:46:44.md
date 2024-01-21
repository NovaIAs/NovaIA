```scheme

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (suma-lista lista)
  (if (null? lista)
      0
      (+ (car lista) (suma-lista (cdr lista)))))

(define (invierte-lista lista)
  (cond
    ((null? lista) '())
    (else (cons (last lista) (invierte-lista (butlast lista))))))

(define (define-macro-suma (nombre-macro)
  (syntax
  (define-macro nombre-macro #:syntax ()
  (lambda ()
  (values 1 2 3)))
(define-macro-suma 'suma-tres-veces)
(suma-tres-veces)

```

Explicación del código:

1. Función factorial: calcula el factorial de un número natural n.

2. Función fibonacci: calcula el n-ésimo número de la secuencia de Fibonacci.

3. Función suma-lista: calcula la suma de los elementos de una lista.

4. Función invierte-lista: invierte el orden de los elementos de una lista.

5. Función define-macro-suma: define una macro que toma tres valores y devuelve su suma.