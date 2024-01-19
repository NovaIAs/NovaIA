```scheme

(define (saludar nombre)
  (display "Hola, ")
  (display nombre)
  (newline))

(define (sumar a b)
  (+ a b))

(define (restar a b)
  (- a b))

(define (multiplicar a b)
  (* a b))

(define (dividir a b)
  (/ a b))

(define (potencia a b)
  (expt a b))

(define (raiz-cuadrada a)
  (sqrt a))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (longitud lista)
  (if (null? lista)
      0
      (+ 1 (longitud (cdr lista)))))

(define (concatenar lista1 lista2)
  (append lista1 lista2))

(define (invertir lista)
  (reverse lista))

(define (ordenar lista)
  (sort lista))

(define (buscar elemento lista)
  (member elemento lista))

(define (filtrar predicado lista)
  (filter predicado lista))

(define (mapear funcion lista)
  (map funcion lista))

(define (reducir funcion valor-inicial lista)
  (foldl funcion valor-inicial lista))

(define (crear-arreglo n)
  (make-vector n))

(define (obtener-elemento arreglo indice)
  (vector-ref arreglo indice))

(define (establecer-elemento arreglo indice valor)
  (vector-set! arreglo indice valor))

(define (crear-hash-map)
  (make-hash))

(define (establecer-valor hash-map clave valor)
  (hash-set! hash-map clave valor))

(define (obtener-valor hash-map clave)
  (hash-ref hash-map clave))

```

Este código define una serie de funciones y procedimientos útiles en Scheme. Las funciones incluyen:

* `saludar`: saluda a una persona por su nombre.
* `sumar`, `restar`, `multiplicar` y `dividir`: realizan las operaciones aritméticas básicas.
* `potencia`: calcula la potencia de un número.
* `raiz-cuadrada`: calcula la raíz cuadrada de un número.
* `factorial`: calcula el factorial de un número.
* `longitud`: calcula la longitud de una lista.
* `concatenar`: concatena dos listas.
* `invertir`: invierte una lista.
* `ordenar`: ordena una lista.
* `buscar`: busca un elemento en una lista.
* `filtrar`: filtra una lista según un predicado.
* `mapear`: aplica una función a cada elemento de una lista.
* `reducir`: reduce una lista a un valor único mediante una función.
* `crear-arreglo`: crea un arreglo de un tamaño especificado.
* `obtener-elemento` y `establecer-elemento`: obtienen y establecen elementos en un arreglo.
* `crear-hash-map`: crea un mapa hash.
* `establecer-valor` y `obtener-valor`: establecen y obtienen valores en un mapa hash.

Estas funciones pueden utilizarse para realizar una amplia variedad de tareas de programación.