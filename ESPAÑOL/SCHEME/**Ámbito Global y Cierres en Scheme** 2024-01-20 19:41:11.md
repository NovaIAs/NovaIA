```scheme
(define ambito-global (make-hash-table))

(define (definir nombre valor)
  (set-hash-table! ambito-global nombre valor))

(define (obtener nombre)
  (hash-table-ref ambito-global nombre))

(define (funcion nombre parametros cuerpo)
  (define (closure)
    (lambda args
      (let loop ((variables parametros)
                (argumentos args))
        (if (null? variables)
            (apply cuerpo argumentos)
            (define (var (car variables))
              (car argumentos))
            (define (arg (cdr argumentos))
              (cdr argumentos))
            (loop (cdr variables)
                  arg)))))
  (define (nombre) closure))

(definir 'suma
  (funcion
    (nombre
      suma)
    (parametros
      x
      y)
    (cuerpo
      (+ x y))))

(definir 'x
  20)

(definir 'y
  25)

(display (suma x y))
```

Explicación:

1. Definimos el ámbito global usando una tabla hash y le damos el nombre `ambito-global`, que contendrá los símbolos y sus valores.

2. Definimos dos procedimientos, `definir` y `obtener`, que interactúan con la tabla hash `ambito-global`. `definir` añade un símbolo y su valor a la tabla, y `obtener` devuelve el valor de un símbolo dado.

3. Creamos un mecanismo de cierres definiendo el procedimiento `funcion`, que recibe un nombre, una lista de parámetros, un cuerpo y devuelve una nueva función que es un cierre. Este cierre tiene una variable privada que contiene los parámetros y el cuerpo de la función.

4. Definimos la función `suma` usando `funcion`, que toma dos parámetros y los suma.

5. Definimos los símbolos `x` e `y` y les damos valores.

6. Por último, imprimimos el resultado de llamar a la función `suma` con los valores de `x` e `y`.