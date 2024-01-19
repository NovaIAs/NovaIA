```fortran
PROGRAMA CALCULAR_FACTORIAL

! Este programa calcula el factorial de un número.

IMPLICIT NONE

! Declaración de las variables

INTEGER :: n, factorial

! Lectura del número

WRITE (*, '(A)') 'Introduce un número:'
READ (*, *) n

! Cálculo del factorial

factorial = 1
DO i = 1, n
    factorial = factorial * i
END DO

! Escritura del resultado

WRITE (*, '(A, I0, A, I0)') 'El factorial de ', n, ' es ', factorial

END PROGRAM CALCULAR_FACTORIAL
```

Explicación del código:

* En la primera línea se indica el nombre del programa.
* La segunda línea indica que se utilizará el formato de datos implícito. Esto significa que no es necesario declarar explícitamente el tipo de datos de las variables.
* La tercera línea declara las variables que se utilizarán en el programa.
* La cuarta línea lee el número del usuario.
* La quinta línea calcula el factorial del número. Para ello, se utiliza un bucle `DO`. El bucle se ejecuta `n` veces, y en cada iteración se multiplica el valor de `factorial` por el valor de `i`.
* La sexta línea escribe el resultado en la pantalla.

Este código es bastante simple, pero es un buen ejemplo de cómo utilizar los bucles y las variables en FORTRAN.