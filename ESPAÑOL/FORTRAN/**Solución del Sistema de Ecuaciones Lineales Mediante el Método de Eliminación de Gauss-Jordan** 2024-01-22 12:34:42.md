```fortran
PROGRAMA MATRIZ
! Este programa resuelve el sistema de ecuaciones lineales Ax = b
! mediante el método de eliminación de Gauss-Jordan.

! Declaración de variables
DIMENSION A(3,3), b(3), x(3)

! Lectura de la matriz A y el vector b
PRINT *, 'Introduzca los elementos de la matriz A:'
DO I = 1, 3
  READ *, A(I,1), A(I,2), A(I,3)
END DO

PRINT *, 'Introduzca los elementos del vector b:'
DO I = 1, 3
  READ *, b(I)
END DO

! Impresión de la matriz A y el vector b
PRINT *, 'Matriz A:'
DO I = 1, 3
  PRINT *, (A(I,J), J = 1, 3)
END DO

PRINT *, 'Vector b:'
DO I = 1, 3
  PRINT *, b(I)
END DO

! Resolución del sistema de ecuaciones lineales
DO I = 1, 3
  ! Pivoteo parcial
  IMAX = I
  DO J = I+1, 3
    IF (ABS(A(J,I)) > ABS(A(IMAX,I))) IMAX = J
  END DO

  IF (IMAX /= I) THEN
    DO J = 1, 3
      TEMP = A(I,J)
      A(I,J) = A(IMAX,J)
      A(IMAX,J) = TEMP
    END DO

    TEMP = b(I)
    b(I) = b(IMAX)
    b(IMAX) = TEMP
  END IF

  ! Eliminación hacia delante
  DO J = I+1, 3
    FACTOR = A(J,I) / A(I,I)
    DO K = 1, 3
      A(J,K) = A(J,K) - FACTOR * A(I,K)
    END DO
    b(J) = b(J) - FACTOR * b(I)
  END DO
END DO

! Eliminación hacia atrás
DO I = 3, 1, -1
  DO J = 1, I-1
    FACTOR = A(J,I) / A(I,I)
    DO K = 1, 3
      A(J,K) = A(J,K) - FACTOR * A(I,K)
    END DO
    b(J) = b(J) - FACTOR * b(I)
  END DO
END DO

! Cálculo del vector x
DO I = 1, 3
  x(I) = b(I) / A(I,I)
END DO

! Impresión del vector x
PRINT *, 'Vector x:'
DO I = 1, 3
  PRINT *, x(I)
END DO

END PROGRAMA MATRIZ
```

**Explicación del código:**

* El programa comienza declarando las variables necesarias.
* A continuación, se lee la matriz **A** y el vector **b** desde la entrada estándar.
* Se imprime la matriz **A** y el vector **b**.
* Se resuelve el sistema de ecuaciones lineales mediante el método de eliminación de Gauss-Jordan.
* Finalmente, se imprime el vector solución **x**.

**Detalles del código:**

* El pivoteo parcial se utiliza para mejorar la estabilidad numérica del método de eliminación de Gauss-Jordan.
* La eliminación hacia delante se utiliza para reducir la matriz **A** a una matriz triangular superior.
* La eliminación hacia atrás se utiliza para resolver la matriz triangular superior y obtener el vector solución **x**.