```fortran
PROGRAMA PRINCIPAL
IMPLICIT NONE

! Declaración de variables
INTEGER :: i, j, k
REAL :: x, y, z
CHARACTER(len=10) :: nombre

! Lectura de datos
PRINT *, "Introduzca su nombre:"
READ *, nombre
PRINT *, "Introduzca tres números:"
READ *, x, y, z

! Cálculos
i = INT(x)
j = INT(y)
k = INT(z)

! Impresión de resultados
PRINT *, "Su nombre es:", nombre
PRINT *, "Los números enteros son:", i, j, k

END PROGRAMA PRINCIPAL
```

**Explicación del código:**

* El programa comienza con la línea `PROGRAMA PRINCIPAL`, que indica que se trata del programa principal.
* La línea `IMPLICIT NONE` indica que todas las variables deben ser declaradas explícitamente.
* Las siguientes líneas declaran las variables que se usarán en el programa.
* La línea `PRINT *, "Introduzca su nombre:"` imprime un mensaje en la consola pidiendo al usuario que introduzca su nombre.
* La línea `READ *, nombre` lee el nombre del usuario y lo almacena en la variable `nombre`.
* La línea `PRINT *, "Introduzca tres números:"` imprime un mensaje en la consola pidiendo al usuario que introduzca tres números.
* La línea `READ *, x, y, z` lee los tres números del usuario y los almacena en las variables `x`, `y` y `z`.
* La línea `i = INT(x)` convierte el número real `x` en un número entero y lo almacena en la variable `i`.
* La línea `j = INT(y)` convierte el número real `y` en un número entero y lo almacena en la variable `j`.
* La línea `k = INT(z)` convierte el número real `z` en un número entero y lo almacena en la variable `k`.
* La línea `PRINT *, "Su nombre es:", nombre` imprime el mensaje "Su nombre es:" en la consola, seguido del nombre del usuario.
* La línea `PRINT *, "Los números enteros son:", i, j, k` imprime el mensaje "Los números enteros son:" en la consola, seguido de los tres números enteros `i`, `j` y `k`.