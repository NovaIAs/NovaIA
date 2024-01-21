```fortran
PROGRAM ProgramaComplejo

! Declaración de variables
INTEGER :: n, i, j, k
REAL :: A(100, 100), B(100, 100), C(100, 100)

! Leer las matrices A y B
PRINT *, "Introduzca el tamaño de las matrices (máximo 100x100):"
READ *, n

PRINT *, "Introduzca los elementos de la matriz A:"
DO i = 1, n
  READ *, (A(i, j), j = 1, n)
END DO

PRINT *, "Introduzca los elementos de la matriz B:"
DO i = 1, n
  READ *, (B(i, j), j = 1, n)
END DO

! Calcular la matriz C = A * B
DO i = 1, n
  DO j = 1, n
    C(i, j) = 0.0
    DO k = 1, n
      C(i, j) = C(i, j) + A(i, k) * B(k, j)
    END DO
  END DO
END DO

! Imprimir la matriz C
PRINT *, "La matriz C es:"
DO i = 1, n
  DO j = 1, n
    PRINT *, C(i, j)
  END DO
END DO

END PROGRAM ProgramaComplejo
```

Explicación del código:

* La primera línea es una declaración de programa que indica que el nombre del programa es "ProgramaComplejo".
* Las líneas 7 a 10 son declaraciones de variables. Se declara una variable entera `n` para almacenar el tamaño de las matrices, y dos matrices bidimensionales reales `A` y `B` para almacenar los elementos de las matrices.
* Las líneas 12 a 16 leen el tamaño de las matrices del usuario.
* Las líneas 18 a 23 leen los elementos de la matriz A del usuario.
* Las líneas 25 a 30 leen los elementos de la matriz B del usuario.
* Las líneas 32 a 46 calculan la matriz C = A * B.
* Las líneas 48 a 53 imprimen la matriz C.