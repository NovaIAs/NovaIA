```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
IMPLICIT NONE
INTEGER :: I, J, K
REAL :: A(10, 10), B(10, 10), C(10, 10)

! Inicialización de matrices
DO I = 1, 10
  DO J = 1, 10
    A(I, J) = 0.0
    B(I, J) = 0.0
    C(I, J) = 0.0
  END DO
END DO

! Llenado de matrices
DO I = 1, 10
  DO J = 1, 10
    A(I, J) = I + J
    B(I, J) = I - J
  END DO
END DO

! Suma de matrices
DO I = 1, 10
  DO J = 1, 10
    C(I, J) = A(I, J) + B(I, J)
  END DO
END DO

! Impresión de matrices
PRINT *, "Matriz A:"
DO I = 1, 10
  DO J = 1, 10
    PRINT *, A(I, J)
  END DO
END DO

PRINT *, "Matriz B:"
DO I = 1, 10
  DO J = 1, 10
    PRINT *, B(I, J)
  END DO
END DO

PRINT *, "Matriz C:"
DO I = 1, 10
  DO J = 1, 10
    PRINT *, C(I, J)
  END DO
END DO

END PROGRAMA PRINCIPAL
```

Explicación del código:

* El código comienza con la declaración de variables, incluyendo la declaración `IMPLICIT NONE` que obliga al programador a declarar todas las variables explícitamente.
* A continuación, se inicializan las matrices `A`, `B` y `C` con valores cero.
* Se llenan las matrices `A` y `B` con valores utilizando el ciclo `DO`.
* Se suma la matriz `A` y la matriz `B` y se almacena el resultado en la matriz `C`.
* Finalmente, se imprimen las tres matrices en la pantalla.