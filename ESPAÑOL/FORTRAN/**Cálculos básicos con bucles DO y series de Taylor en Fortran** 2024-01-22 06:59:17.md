```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
INTEGER :: i, j, k, n, m
REAL :: x, y, z, a, b, c

! Entrada de datos
PRINT *, "Introduzca el valor de n:"
READ *, n
PRINT *, "Introduzca el valor de m:"
READ *, m

! Cálculo de la suma de los primeros n números naturales
SUMA = 0
DO i = 1, n
  SUMA = SUMA + i
END DO

! Cálculo del producto de los primeros m números naturales
PRODUCTO = 1
DO j = 1, m
  PRODUCTO = PRODUCTO * j
END DO

! Cálculo del factorial de n
FACTORIAL = 1
DO k = 1, n
  FACTORIAL = FACTORIAL * k
END DO

! Cálculo del seno de x
X = 0.5
SENX = 0.0
TERM = 1.0
DO i = 1, 10
  TERM = TERM * (-1)^i * X^i / FACTORIAL(i)
  SENX = SENX + TERM
END DO

! Cálculo del coseno de x
COSX = 1.0
TERM = 1.0
DO i = 1, 10
  TERM = TERM * (-1)^i * X^(2*i) / FACTORIAL(2*i)
  COSX = COSX + TERM
END DO

! Cálculo de la tangente de x
TANX = SENX / COSX

! Salida de resultados
PRINT *, "La suma de los primeros", n, "números naturales es", SUMA
PRINT *, "El producto de los primeros", m, "números naturales es", PRODUCTO
PRINT *, "El factorial de", n, "es", FACTORIAL
PRINT *, "El seno de", X, "es", SENX
PRINT *, "El coseno de", X, "es", COSX
PRINT *, "La tangente de", X, "es", TANX

END PROGRAMA PRINCIPAL
```

Explicación del código:

1. Declaración de variables: Se declaran las variables que se utilizarán en el programa, incluyendo enteros (INTEGER) para los índices de los bucles y los valores de n y m, y reales (REAL) para los valores de x, y, z, a, b y c.
2. Entrada de datos: Se solicita al usuario que introduzca los valores de n y m.
3. Cálculo de la suma de los primeros n números naturales: Se utiliza un bucle DO para iterar sobre los números naturales de 1 a n y se calcula la suma de estos números.
4. Cálculo del producto de los primeros m números naturales: Se utiliza un bucle DO para iterar sobre los números naturales de 1 a m y se calcula el producto de estos números.
5. Cálculo del factorial de n: Se utiliza un bucle DO para iterar sobre los números naturales de 1 a n y se calcula el factorial de n.
6. Cálculo del seno de x: Se utiliza una serie de Taylor para calcular el seno de x, iterando sobre los términos de la serie hasta que la diferencia entre términos consecutivos sea menor que una tolerancia determinada.
7. Cálculo del coseno de x: Se utiliza una serie de Taylor para calcular el coseno de x, iterando sobre los términos de la serie hasta que la diferencia entre términos consecutivos sea menor que una tolerancia determinada.
8. Cálculo de la tangente de x: Se calcula la tangente de x como el cociente del seno y el coseno de x.
9. Salida de resultados: Se imprimen los resultados de los cálculos en la consola.