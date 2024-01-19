```fortran
PROGRAMA PRINCIPAL

IMPLICIT NONE

! DECLARACIÓN DE VARIABLES

INTEGER :: N, I, J, K
REAL :: A(10, 10), B(10, 10), C(10, 10)

! LECTURA DE DATOS

PRINT *, "Ingrese el número de filas y columnas de las matrices:"
READ *, N

DO I = 1, N
  DO J = 1, N
    PRINT *, "Ingrese el elemento A(", I, ",", J, "):"
    READ *, A(I, J)
  END DO
END DO

DO I = 1, N
  DO J = 1, N
    PRINT *, "Ingrese el elemento B(", I, ",", J, "):"
    READ *, B(I, J)
  END DO
END DO

! MULTIPLICACIÓN DE MATRICES

DO I = 1, N
  DO J = 1, N
    C(I, J) = 0.0
    DO K = 1, N
      C(I, J) = C(I, J) + A(I, K) * B(K, J)
    END DO
  END DO
END DO

! IMPRESIÓN DE RESULTADOS

PRINT *, "La matriz resultante es:"
DO I = 1, N
  DO J = 1, N
    PRINT *, C(I, J)
  END DO
END DO

END PROGRAMA PRINCIPAL
```

**Explicación del código:**

Este código en FORTRAN realiza la multiplicación de dos matrices cuadradas de orden N.

* **Líneas 1-4:** Declaración de variables.

* **Líneas 6-13:** Lectura de datos. El usuario debe ingresar el número de filas y columnas de las matrices, así como los elementos de cada matriz.

* **Líneas 15-22:** Multiplicación de matrices. Se utiliza un bucle triple para multiplicar cada elemento de la matriz A por cada elemento de la matriz B, y el resultado se almacena en la matriz C.

* **Líneas 24-31:** Impresión de resultados. Se imprime la matriz resultante C.