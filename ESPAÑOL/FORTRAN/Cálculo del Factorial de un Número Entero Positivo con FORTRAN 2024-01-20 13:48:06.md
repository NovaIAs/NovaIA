```fortran
PROGRAMA EJEMPLO
! Este programa calcula el factorial de un número entero positivo.

! Declaración de variables
IMPLICIT NONE

! Tipo de dato para los números enteros
INTEGER :: n, factorial

! Lee el número entero positivo
PRINT *, "Introduzca un número entero positivo:"
READ *, n

! Inicializa el factorial a 1
factorial = 1

! Calcula el factorial
DO i = 1, n
    factorial = factorial * i
END DO

! Imprime el resultado
PRINT *, "El factorial de", n, "es", factorial

END PROGRAM EJEMPLO
```

Este código calcula el factorial de un número entero positivo utilizando un bucle `DO`. El factorial de un número es el producto de todos los números enteros positivos desde 1 hasta ese número. Por ejemplo, el factorial de 5 es 5 * 4 * 3 * 2 * 1 = 120.

El código comienza declarando las variables que se van a utilizar. La variable `n` es el número entero positivo que se va a calcular el factorial. La variable `factorial` es el resultado del cálculo.

A continuación, el código lee el número entero positivo del usuario utilizando la instrucción `READ`.

Después, el código inicializa la variable `factorial` a 1. Esto es necesario porque el factorial de 0 es 1.

A continuación, el código calcula el factorial utilizando un bucle `DO`. El bucle `DO` se ejecuta desde 1 hasta `n`, y en cada iteración del bucle, el valor de `factorial` se multiplica por el valor de `i`.

Finalmente, el código imprime el resultado utilizando la instrucción `PRINT`.