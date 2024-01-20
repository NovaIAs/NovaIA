```fortran
PROGRAM SOLUCION_ECUACIONES
! Este programa resuelve un sistema de ecuaciones lineales usando el método de eliminación de Gauss-Jordan.

! Declaración de variables
INTEGER, PARAMETER :: N = 3 ! Número de ecuaciones y variables
REAL(8) :: A(N,N) ! Matriz de coeficientes
REAL(8) :: B(N) ! Vector de términos independientes
REAL(8) :: X(N) ! Vector de soluciones

! Entrada de datos
PRINT *, "Introduzca la matriz de coeficientes:"
DO I = 1, N
  DO J = 1, N
    READ *, A(I,J)
  END DO
END DO

PRINT *, "Introduzca el vector de términos independientes:"
DO I = 1, N
  READ *, B(I)
END DO

! Resolución del sistema de ecuaciones
! Conversión de la matriz aumentada a forma escalonada reducida
DO I = 1, N
  ! Pivoteo parcial
  IPIV = I
  DO J = I+1, N
    IF (ABS(A(J,I)) > ABS(A(IPIV,I))) IPIV = J
  END DO

  ! Intercambio de filas
  IF (IPIV /= I) THEN
    DO J = 1, N
      TEMP = A(I,J)
      A(I,J) = A(IPIV,J)
      A(IPIV,J) = TEMP
    END DO

    TEMP = B(I)
    B(I) = B(IPIV)
    B(IPIV) = TEMP
  END IF

  ! Normalización de la fila actual
  DIVISOR = A(I,I)
  DO J = 1, N
    A(I,J) = A(I,J) / DIVISOR
  END DO
  B(I) = B(I) / DIVISOR

  ! Eliminación de los coeficientes por debajo de la diagonal principal
  DO J = I+1, N
    MULTIPLIER = A(J,I)
    DO K = 1, N
      A(J,K) = A(J,K) - MULTIPLIER * A(I,K)
    END DO
    B(J) = B(J) - MULTIPLIER * B(I)
  END DO
END DO

! Extracción de las soluciones
DO I = N, 1, -1
  X(I) = B(I)
  DO J = I+1, N
    X(I) = X(I) - A(I,J) * X(J)
  END DO
END DO

! Salida de resultados
PRINT *, "Las soluciones del sistema de ecuaciones son:"
DO I = 1, N
  PRINT *, "x", I, " = ", X(I)
END DO

END PROGRAM SOLUCION_ECUACIONES
```

**Explicación del código:**

1. El programa comienza declarando las variables necesarias:

    * `N`: El número de ecuaciones y variables en el sistema de ecuaciones.
    * `A`: La matriz de coeficientes de las ecuaciones.
    * `B`: El vector de términos independientes de las ecuaciones.
    * `X`: El vector de soluciones de las ecuaciones.

2. A continuación, el programa solicita al usuario que introduzca la matriz de coeficientes y el vector de términos independientes.

3. El programa procede a resolver el sistema de ecuaciones utilizando el método de eliminación de Gauss-Jordan. Este método consiste en convertir la matriz aumentada del sistema de ecuaciones (la matriz de coeficientes junto con el vector de términos independientes) en una matriz escalonada reducida. Una matriz escalonada reducida es una matriz en la que:

    * Cada fila tiene un pivote (un elemento distinto de cero) en la primera columna no nula.
    * Cada pivote está por encima y a la derecha de los pivotes de las filas anteriores.
    * Todos los elementos por debajo de un pivote son cero.

4. Una vez que la matriz aumentada se ha convertido en una matriz escalonada reducida, es fácil extraer las soluciones del sistema de ecuaciones. Las soluciones se encuentran en la última columna de la matriz escalonada reducida.

5. Finalmente, el programa imprime las soluciones del sistema de ecuaciones en la pantalla.