```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
IMPLICIT NONE
INTEGER :: N, M, I, J, K, L
REAL :: A(100, 100), B(100, 100), C(100, 100)

! Entrada de datos
PRINT *, "Introduzca el número de filas de las matrices:"
READ *, N
PRINT *, "Introduzca el número de columnas de las matrices:"
READ *, M

! Rellenado de las matrices A y B con números aleatorios
DO I = 1, N
    DO J = 1, M
        A(I, J) = RANDOM_NUMBER()
        B(I, J) = RANDOM_NUMBER()
    END DO
END DO

! Cálculo de la matriz C como la suma de A y B
DO I = 1, N
    DO J = 1, M
        C(I, J) = A(I, J) + B(I, J)
    END DO
END DO

! Impresión de la matriz C
PRINT *, "La matriz C es:"
DO I = 1, N
    DO J = 1, M
        PRINT *, C(I, J)
    END DO
END DO

END PROGRAMA PRINCIPAL
```

Explicación del código:

* El programa comienza con la declaración de las variables que se van a utilizar en el programa.
* El usuario debe introducir el número de filas y columnas de las matrices A y B.
* Las matrices A y B se rellenan con números aleatorios utilizando la función `RANDOM_NUMBER()`.
* La matriz C se calcula como la suma de las matrices A y B.
* Finalmente, se imprime la matriz C.