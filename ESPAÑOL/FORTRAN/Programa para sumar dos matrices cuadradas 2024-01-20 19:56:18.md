```fortran
PROGRAMA PRINCIPAL

! DECLARACIÓN DE VARIABLES
IMPLICIT NONE
INTEGER :: N, I, J, K, L
REAL :: A(100, 100), B(100, 100), C(100, 100)

! INGRESO DE DATOS
PRINT *, "INGRESE EL TAMAÑO DE LAS MATRICES (MÁXIMO 100)": READ *, N
DO I = 1, N
    PRINT *, "INGRESE LA FILA ", I, " DE LA MATRIZ A:"
    READ *, (A(I, J), J = 1, N)
END DO

DO I = 1, N
    PRINT *, "INGRESE LA FILA ", I, " DE LA MATRIZ B:"
    READ *, (B(I, J), J = 1, N)
END DO

! CÁLCULO DE LA SUMA DE LAS MATRICES
DO I = 1, N
    DO J = 1, N
        C(I, J) = A(I, J) + B(I, J)
    END DO
END DO

! IMPRESIÓN DE RESULTADOS
PRINT *, "LA SUMA DE LAS MATRICES ES:"
DO I = 1, N
    DO J = 1, N
        PRINT *, C(I, J)
    END DO
END DO

END PROGRAMA PRINCIPAL

```

Este código en FORTRAN realiza la suma de dos matrices cuadradas de tamaño N x N.

El programa primero solicita al usuario que introduzca el tamaño de las matrices (N). Luego, solicita al usuario que introduzca los elementos de las matrices A y B.

A continuación, el programa calcula la suma de las matrices A y B y almacena el resultado en la matriz C.

Finalmente, el programa imprime la matriz C, que contiene la suma de las matrices A y B.