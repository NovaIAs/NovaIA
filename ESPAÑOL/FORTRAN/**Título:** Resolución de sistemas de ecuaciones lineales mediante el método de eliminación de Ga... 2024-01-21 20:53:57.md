```fortran
PROGRAMA PRINCIPAL
! Este programa resuelve un sistema de ecuaciones lineales usando el método de eliminación de Gauss-Jordan.

! Declaración de variables
REAL :: A(3,3), B(3), X(3) ! Matriz de coeficientes, vector de términos independientes y vector de incógnitas
INTEGER :: i, j, k ! Índices

! Introducción de la matriz de coeficientes y el vector de términos independientes
PRINT *, "Introduzca la matriz de coeficientes:"
DO i = 1, 3
  DO j = 1, 3
    READ *, A(i,j)
  END DO
END DO

PRINT *, "Introduzca el vector de términos independientes:"
DO i = 1, 3
  READ *, B(i)
END DO

! Impresión de la matriz de coeficientes y el vector de términos independientes
PRINT *, "Matriz de coeficientes:"
DO i = 1, 3
  DO j = 1, 3
    PRINT *, A(i,j),
  END DO
  PRINT *
END DO

PRINT *, "Vector de términos independientes:"
DO i = 1, 3
  PRINT *, B(i)
END DO

! Eliminación de Gauss-Jordan
DO i = 1, 3
  ! Normalización de la fila actual
  FACTOR = A(i,i)
  DO j = 1, 3
    A(i,j) = A(i,j) / FACTOR
  END DO
  B(i) = B(i) / FACTOR

  ! Eliminación de los elementos de la columna actual en las demás filas
  DO k = 1, 3
    IF (k /= i) THEN
      FACTOR = A(k,i)
      DO j = 1, 3
        A(k,j) = A(k,j) - FACTOR * A(i,j)
      END DO
      B(k) = B(k) - FACTOR * B(i)
    END IF
  END DO
END DO

! Impresión de la matriz de coeficientes y el vector de términos independientes después de la eliminación de Gauss-Jordan
PRINT *, "Matriz de coeficientes después de la eliminación de Gauss-Jordan:"
DO i = 1, 3
  DO j = 1, 3
    PRINT *, A(i,j),
  END DO
  PRINT *
END DO

PRINT *, "Vector de términos independientes después de la eliminación de Gauss-Jordan:"
DO i = 1, 3
  PRINT *, B(i)
END DO

! Obtención de las incógnitas
DO i = 1, 3
  X(i) = B(i)
END DO

! Impresión de las incógnitas
PRINT *, "Incógnitas:"
DO i = 1, 3
  PRINT *, X(i)
END DO

END PROGRAMA PRINCIPAL
```

Explicación del código:

* El programa principal es el punto de entrada del programa.
* Las variables `A`, `B` y `X` se utilizan para almacenar la matriz de coeficientes, el vector de términos independientes y el vector de incógnitas, respectivamente.
* El programa solicita al usuario que introduzca la matriz de coeficientes y el vector de términos independientes.
* A continuación, imprime la matriz de coeficientes y el vector de términos independientes.
* El programa realiza la eliminación de Gauss-Jordan para resolver el sistema de ecuaciones lineales.
* Después de la eliminación de Gauss-Jordan, imprime la matriz de coeficientes y el vector de términos independientes.
* Finalmente, calcula las incógnitas y las imprime.