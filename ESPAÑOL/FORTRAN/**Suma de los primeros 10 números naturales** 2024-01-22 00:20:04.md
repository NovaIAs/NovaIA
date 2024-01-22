```fortran
PROGRAMA EJEMPLO
! Este programa calcula la suma de los primeros 10 números naturales.

IMPLICIT NONE

! Declaración de variables
INTEGER :: i, suma

! Inicialización de variables
i = 1
suma = 0

! Bucle para sumar los primeros 10 números naturales
DO i = 1, 10
  suma = suma + i
END DO

! Impresión del resultado
PRINT *, "La suma de los primeros 10 números naturales es", suma

END PROGRAM EJEMPLO
```

Explicación del código:

* La primera línea del programa es la instrucción `PROGRAMA EJEMPLO`, que indica el nombre del programa.
* La segunda línea es un comentario, que se utiliza para explicar el propósito del programa.
* La tercera línea es la instrucción `IMPLICIT NONE`, que indica al compilador que no se deben utilizar variables implícitas. Esto significa que todas las variables deben ser declaradas explícitamente.
* La cuarta línea declara las variables `i` y `suma`, que se utilizan para llevar la cuenta del número actual y de la suma de los números, respectivamente.
* La quinta línea inicializa la variable `i` a 1 y la variable `suma` a 0.
* La sexta línea es el bucle `DO`, que se utiliza para sumar los primeros 10 números naturales. El bucle se ejecuta desde `i = 1` hasta `i = 10`, con un incremento de 1 en cada iteración.
* La séptima línea es la instrucción `suma = suma + i`, que suma el valor actual de `i` a la variable `suma`.
* La octava línea es la instrucción `PRINT *`, que imprime el resultado de la suma en la consola.
* La novena línea es la instrucción `END DO`, que indica el final del bucle `DO`.
* La décima línea es la instrucción `END PROGRAM EJEMPLO`, que indica el final del programa.