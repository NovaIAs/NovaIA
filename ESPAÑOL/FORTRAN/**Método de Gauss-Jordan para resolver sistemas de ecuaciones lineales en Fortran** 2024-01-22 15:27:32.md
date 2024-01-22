```fortran
PROGRAMA PRINCIPAL
! Este programa resuelve un sistema de ecuaciones lineales utilizando el método de Gauss-Jordan.

! Declaración de variables
INTEGER :: n, i, j, k
REAL :: A(n, n), b(n), x(n)

! Lectura de la matriz A y del vector b
PRINT *, "Ingrese el número de ecuaciones:"
READ *, n
PRINT *, "Ingrese los coeficientes de la matriz A:"
DO i = 1, n
    DO j = 1, n
        READ *, A(i, j)
    END DO
END DO
PRINT *, "Ingrese los valores del vector b:"
DO i = 1, n
    READ *, b(i)
END DO

! Impresión de la matriz A y del vector b
PRINT *, "Matriz A:"
DO i = 1, n
    DO j = 1, n
        PRINT *, A(i, j),
    END DO
    PRINT *
END DO
PRINT *, "Vector b:"
DO i = 1, n
    PRINT *, b(i)
END DO

! Resolución del sistema de ecuaciones
DO k = 1, n
    ! Normalización de la fila k
    r = A(k, k)
    DO j = 1, n
        A(k, j) = A(k, j) / r
    END DO
    b(k) = b(k) / r

    ! Eliminación de los elementos por debajo de la diagonal k
    DO i = k + 1, n
        r = A(i, k)
        DO j = 1, n
            A(i, j) = A(i, j) - r * A(k, j)
        END DO
        b(i) = b(i) - r * b(k)
    END DO
END DO

! Sustitución hacia atrás
x(n) = b(n)
DO i = n - 1, 1, -1
    s = 0.0
    DO j = i + 1, n
        s = s + A(i, j) * x(j)
    END DO
    x(i) = (b(i) - s) / A(i, i)
END DO

! Impresión de la solución
PRINT *, "Solución:"
DO i = 1, n
    PRINT *, x(i)
END DO

END PROGRAMA PRINCIPAL
```

Este código resuelve un sistema de ecuaciones lineales utilizando el método de Gauss-Jordan, que es un método numérico para resolver sistemas de ecuaciones lineales.

El código primero lee la matriz A, el vector b y el número de ecuaciones. Luego, normaliza la fila k y elimina los elementos por debajo de la diagonal k. A continuación, realiza una sustitución hacia atrás para encontrar los valores de las variables x. Finalmente, imprime la solución.

El código está en español y está muy bien comentado, por lo que es fácil de entender. También es muy eficiente, ya que utiliza el método de Gauss-Jordan, que es uno de los métodos más eficientes para resolver sistemas de ecuaciones lineales.