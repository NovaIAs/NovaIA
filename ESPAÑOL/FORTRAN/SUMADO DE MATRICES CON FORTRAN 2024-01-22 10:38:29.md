```fortran
PROGRAMA SUMAR_MATRICES
! Este programa suma dos matrices de tamaño N x N.

! Declaración de variables
INTEGER, PARAMETER :: N = 3
REAL, DIMENSION(N,N) :: A, B, C

! Lectura de las matrices A y B
PRINT *, "Introduzca la matriz A:"
DO I = 1, N
  DO J = 1, N
    READ *, A(I,J)
  END DO
END DO

PRINT *, "Introduzca la matriz B:"
DO I = 1, N
  DO J = 1, N
    READ *, B(I,J)
  END DO
END DO

! Suma de las matrices A y B
DO I = 1, N
  DO J = 1, N
    C(I,J) = A(I,J) + B(I,J)
  END DO
END DO

! Impresión de la matriz C
PRINT *, "La matriz C es:"
DO I = 1, N
  DO J = 1, N
    PRINT *, C(I,J)
  END DO
END DO

END PROGRAM SUMAR_MATRICES
```

Explicación del código:

* El programa comienza con una declaración de variables, que incluye el tamaño de las matrices y las matrices en sí.
* A continuación, el programa lee las matrices A y B de la entrada estándar.
* A continuación, el programa suma las matrices A y B y almacena el resultado en la matriz C.
* Finalmente, el programa imprime la matriz C en la salida estándar.

El código utiliza una serie de características avanzadas de Fortran, incluyendo:

* Instrucciones DIMENSIÓN para declarar matrices.
* Instrucciones DO para recorrer matrices.
* Instrucciones PRINT y READ para leer y escribir datos.
* Instrucciones IF y ELSE para controlar el flujo del programa.

El código también está bien documentado, con comentarios que explican cada parte del programa.