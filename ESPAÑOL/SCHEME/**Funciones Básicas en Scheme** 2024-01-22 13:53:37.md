```scheme
(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

(define (cuadrado x)
  (* x x))

(define (suma-cuadrados n)
  (if (= n 0)
    0
    (+ (cuadrado n) (suma-cuadrados (- n 1)))))

(define (fibonacci n)
  (if (< n 2)
    n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (primo? n)
  (if (< n 2)
    false
    (let loop ((x 2))
      (if (or (>= x (sqrt n)) (= n 1))
        true
        (if (= (remainder n x) 0)
          false
          (loop (+ x 1)))))

(define (lista-primos n)
  (if (< n 2)
    '()
    (if (primo? n)
      (cons n (lista-primos (- n 1)))
      (lista-primos (- n 1)))))

(define (invertir-lista lista)
  (if (null? lista)
    '()
    (cons (car lista) (invertir-lista (cdr lista)))))

(define (ordenar-lista lista)
  (if (null? lista)
    '()
    (let loop ((lista-ordenada '()) (resto-lista lista))
      (if (null? resto-lista)
        lista-ordenada
        (let ((menor-o-igual (null? lista-ordenada)) (x (car resto-lista)))
          (loop (if menor-o-igual
                  (cons x lista-ordenada)
                  (cons (car lista-ordenada) (ordenar-lista (cons x (cdr lista-ordenada)))))
                (cdr resto-lista)))))))

(define (es-palindromo? cadena)
  (let ((cadena-invertida (invertir-lista cadena)))
    (= cadena cadena-invertida)))

(define (concatenar-listas lista1 lista2)
  (if (null? lista1)
    lista2
    (cons (car lista1) (concatenar-listas (cdr lista1) lista2))))

(define (encontrar-elemento lista elemento-a-encontrar)
  (if (null? lista)
    false
    (if (= (car lista) elemento-a-encontrar)
      true
      (encontrar-elemento (cdr lista) elemento-a-encontrar))))

(define (eliminar-elemento lista elemento-a-eliminar)
  (if (null? lista)
    '()
    (if (= (car lista) elemento-a-eliminar)
      (cdr lista)
      (cons (car lista) (eliminar-elemento (cdr lista) elemento-a-eliminar)))))

(define (sustituir-elemento lista elemento-a-sustituir elemento-sustituto)
  (if (null? lista)
    '()
    (if (= (car lista) elemento-a-sustituir)
      (cons elemento-sustituto (sustituir-elemento (cdr lista) elemento-a-sustituir elemento-sustituto))
      (cons (car lista) (sustituir-elemento (cdr lista) elemento-a-sustituir elemento-sustituto)))))

(define (ordenar-lista-por-longitud lista)
  (if (null? lista)
    '()
    (let loop ((lista-ordenada '()) (resto-lista lista))
      (if (null? resto-lista)
        lista-ordenada
        (let ((menor-o-igual (null? lista-ordenada)) (x (car resto-lista)) (longitud-x (length x)))
          (loop (if menor-o-igual
                  (cons x lista-ordenada)
                  (cons (car lista-ordenada) (ordenar-lista-por-longitud (cons x (cdr lista-ordenada)))))
                (cdr resto-lista)))))))
```

Explicación:

- `factorial`: Calcula el factorial de un número.
- `cuadrado`: Calcula el cuadrado de un número.
- `suma-cuadrados`: Calcula la suma de los cuadrados de los números desde 1 hasta un número dado.
- `fibonacci`: Calcula el n-ésimo número de la secuencia de Fibonacci.
- `primo?`: Comprueba si un número es primo.
- `lista-primos`: Genera una lista de los números primos hasta un número dado.
- `invertir-lista`: Invierte una lista.
- `ordenar-lista`: Ordena una lista de números en orden ascendente.
- `es-palindromo?`: Comprueba si una cadena es un palíndromo.
- `concatenar-listas`: Concatena dos listas en una sola.
- `encontrar-elemento`: Busca un elemento en una lista y devuelve true si lo encuentra, false en caso contrario.
- `eliminar-elemento`: Elimina un elemento de una lista y devuelve la lista resultante.
- `sustituir-elemento`: Sustituye un elemento de una lista por otro y devuelve la lista resultante.
- `ordenar-lista-por-longitud`: Ordena una lista de listas por su longitud en orden ascendente.