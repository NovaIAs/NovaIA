```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
INTEGER :: i, j, k, n, m
REAL :: x, y, z, a, b, c

! Lectura de datos
PRINT *, "Introduzca el número de filas:"
READ *, n
PRINT *, "Introduzca el número de columnas:"
READ *, m
PRINT *, "Introduzca los elementos de la matriz:"
DO i = 1, n
  DO j = 1, m
    READ *, a(i, j)
  END DO
END DO

! Cálculo de la matriz transpuesta
DO i = 1, n
  DO j = 1, m
    b(j, i) = a(i, j)
  END DO
END DO

! Cálculo del producto escalar de dos vectores
PRINT *, "Introduzca los elementos del primer vector:"
DO i = 1, m
  READ *, x(i)
END DO
PRINT *, "Introduzca los elementos del segundo vector:"
DO i = 1, m
  READ *, y(i)
END DO
z = 0.0
DO i = 1, m
  z = z + x(i) * y(i)
END DO

! Cálculo de la matriz inversa
DO i = 1, n
  DO j = 1, m
    c(i, j) = 0.0
    IF (i == j) THEN
      c(i, j) = 1.0
    END IF
  END DO
END DO

DO k = 1, n
  a1 = a(k, k)
  DO i = 1, n
    a(i, k) = a(i, k) / a1
    c(i, k) = c(i, k) / a1
  END DO
  DO j = 1, n
    IF (j /= k) THEN
      a2 = a(j, k)
      DO i = 1, n
        a(i, j) = a(i, j) - a2 * a(i, k)
        c(i, j) = c(i, j) - a2 * c(i, k)
      END DO
    END IF
  END DO
END DO

! Impresión de resultados
PRINT *, "Matriz original:"
DO i = 1, n
  DO j = 1, m
    PRINT *, a(i, j)
  END DO
END DO

PRINT *, "Matriz transpuesta:"
DO i = 1, m
  DO j = 1, n
    PRINT *, b(i, j)
  END DO
END DO

PRINT *, "Producto escalar de los vectores:"
PRINT *, z

PRINT *, "Matriz inversa:"
DO i = 1, n
  DO j = 1, m
    PRINT *, c(i, j)
  END DO
END DO

END PROGRAMA PRINCIPAL
```

Este código realiza las siguientes operaciones:

1. Lee los datos de la matriz original.
2. Calcula la matriz transpuesta.
3. Calcula el producto escalar de dos vectores.
4. Calcula la matriz inversa.
5. Imprime los resultados.

El código está escrito en FORTRAN, un lenguaje de programación de alto nivel que se utiliza para resolver problemas científicos y de ingeniería. FORTRAN es un lenguaje muy potente y eficiente, pero también es muy complejo. El código que se ha proporcionado es sólo un ejemplo de cómo se puede utilizar FORTRAN para resolver problemas matemáticos.