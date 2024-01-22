```fortran
PROGRAM ejemplo

! Declaración de variables
INTEGER :: i, j, k, l, m
REAL :: a, b, c, d, e
CHARACTER(len=10) :: nombre

! Inicio del programa principal
PRINT *, "Hola, mundo!"

! Bucle DO anidado
DO i = 1, 10
  DO j = 1, 10
    DO k = 1, 10
      DO l = 1, 10
        DO m = 1, 10
          a = i + j + k + l + m
          b = a * a
          c = b * b
          d = c * c
          e = d * d
          PRINT *, "i =", i, "j =", j, "k =", k, "l =", l, "m =", m, "a =", a, "b =", b, "c =", c, "d =", d, "e =", e
        END DO
      END DO
    END DO
  END DO
END DO

! Sentencia de lectura
READ *, nombre

! Sentencia de escritura
WRITE (*, *) "Hola, ", nombre, "!"

! Fin del programa
END PROGRAM ejemplo
```

Explicación del código:

* El programa comienza con una sentencia `PROGRAM` que especifica el nombre del programa, en este caso `ejemplo`.
* A continuación, se declaran las variables que se utilizarán en el programa. En este caso, se declaran las siguientes variables:
    * `i`, `j`, `k`, `l` y `m` son variables enteras (de tipo `INTEGER`).
    * `a`, `b`, `c`, `d` y `e` son variables reales (de tipo `REAL`).
    * `nombre` es una variable de caracteres (de tipo `CHARACTER`) de longitud 10.
* El programa principal comienza con una sentencia `PRINT` que imprime el mensaje "Hola, mundo!" en la consola.
* A continuación, se utiliza un bucle `DO` anidado para iterar sobre los valores de las variables `i`, `j`, `k`, `l` y `m`.
* Dentro de cada iteración del bucle `DO`, se calculan los valores de las variables `a`, `b`, `c`, `d` y `e`.
* A continuación, se utiliza una sentencia `PRINT` para imprimir los valores de las variables `i`, `j`, `k`, `l`, `m`, `a`, `b`, `c`, `d` y `e` en la consola.
* Después del bucle `DO`, se utiliza una sentencia `READ` para leer el valor de la variable `nombre` desde la consola.
* A continuación, se utiliza una sentencia `WRITE` para imprimir el mensaje "Hola, ", seguido del valor de la variable `nombre`, seguido del mensaje "!" en la consola.
* El programa finaliza con una sentencia `END PROGRAM` que indica el final del programa.