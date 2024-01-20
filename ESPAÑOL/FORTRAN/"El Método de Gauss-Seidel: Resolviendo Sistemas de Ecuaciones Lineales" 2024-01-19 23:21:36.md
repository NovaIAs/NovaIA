```fortran
PROGRAMA PRINCIPAL
! Este programa resuelve el sistema de ecuaciones lineales Ax = b mediante el método de Gauss-Seidel.

! Declaración de variables
INTEGER :: n, i, j, k, l, m
REAL :: A(n, n), b(n), x(n), error, tolerancia

! Lectura de la dimensión de la matriz A
PRINT *, 'Introduzca la dimensión de la matriz A:'
READ *, n

! Lectura de la matriz A y del vector b
PRINT *, 'Introduzca los elementos de la matriz A:'
DO i = 1, n
  DO j = 1, n
    READ *, A(i, j)
  END DO
END DO

PRINT *, 'Introduzca los elementos del vector b:'
DO i = 1, n
  READ *, b(i)
END DO

! Inicialización del vector x
DO i = 1, n
  x(i) = 0.0
END DO

! Cálculo de la solución mediante el método de Gauss-Seidel
DO k = 1, 100
  DO i = 1, n
    suma = 0.0
    DO j = 1, i-1
      suma = suma + A(i, j) * x(j)
    END DO
    DO j = i+1, n
      suma = suma + A(i, j) * x(j)
    END DO
    x(i) = (b(i) - suma) / A(i, i)
  END DO

  ! Cálculo del error
  error = 0.0
  DO i = 1, n
    suma = 0.0
    DO j = 1, n
      suma = suma + A(i, j) * x(j)
    END DO
    error = error + ABS(suma - b(i))
  END DO

  ! Comprobación de la convergencia
  IF (error < tolerancia) THEN
    EXIT
  END IF
END DO

! Impresión de la solución
PRINT *, 'La solución es:'
DO i = 1, n
  PRINT *, x(i)
END DO

END PROGRAMA PRINCIPAL
```

Explicación del código:

* El programa comienza leyendo la dimensión de la matriz A y los elementos de la matriz A y del vector b.
* A continuación, se inicializa el vector x a cero.
* El método de Gauss-Seidel se utiliza para calcular la solución del sistema de ecuaciones lineales Ax = b. El algoritmo itera hasta que el error es menor que una tolerancia especificada.
* En cada iteración, el algoritmo calcula el nuevo valor de cada componente de x utilizando los valores actuales de las demás componentes de x.
* Una vez que el algoritmo ha convergido, se imprime la solución.