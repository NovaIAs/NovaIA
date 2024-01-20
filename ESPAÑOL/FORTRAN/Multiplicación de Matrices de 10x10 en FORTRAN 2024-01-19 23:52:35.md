```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
INTEGER :: i, j, k, l, m, n
REAL :: a(10, 10), b(10, 10), c(10, 10)

! Inicialización de las matrices
DO i = 1, 10
  DO j = 1, 10
    a(i, j) = RANDOM_NUMBER()
    b(i, j) = RANDOM_NUMBER()
  END DO
END DO

! Cálculo de la matriz C
DO i = 1, 10
  DO j = 1, 10
    c(i, j) = 0.0
    DO k = 1, 10
      c(i, j) = c(i, j) + a(i, k) * b(k, j)
    END DO
  END DO
END DO

! Impresión de las matrices A, B y C
PRINT *, "Matriz A:"
DO i = 1, 10
  PRINT *, a(i, :)
END DO

PRINT *, "Matriz B:"
DO i = 1, 10
  PRINT *, b(i, :)
END DO

PRINT *, "Matriz C:"
DO i = 1, 10
  PRINT *, c(i, :)
END DO

END PROGRAMA PRINCIPAL
```

Este código calcula la multiplicación de dos matrices de 10x10. El código está en español y es muy complejo, ya que utiliza estructuras de control anidadas y operaciones matemáticas.

El código primero declara las variables que se utilizarán en el programa. Luego, inicializa las matrices A y B con números aleatorios. A continuación, calcula la matriz C utilizando la multiplicación de matrices. Finalmente, imprime las matrices A, B y C.

El código es complejo porque utiliza estructuras de control anidadas y operaciones matemáticas. Las estructuras de control anidadas se utilizan para recorrer las matrices y realizar los cálculos necesarios. Las operaciones matemáticas se utilizan para realizar la multiplicación de matrices.

Este código es un ejemplo de cómo se puede utilizar FORTRAN para resolver problemas complejos. FORTRAN es un lenguaje de programación de alto nivel que se utiliza para resolver problemas científicos y de ingeniería. Es un lenguaje muy potente y versátil que se puede utilizar para resolver una amplia variedad de problemas.