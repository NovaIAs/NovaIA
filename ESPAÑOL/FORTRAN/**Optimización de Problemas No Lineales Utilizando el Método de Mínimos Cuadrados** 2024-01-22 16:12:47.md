```fortran
PROGRAMA PRINCIPAL

! Este programa resuelve un problema de optimización no lineal utilizando el método de los mínimos cuadrados.

! Declaración de variables

REAL :: x, y, z
INTEGER :: i, j, k
REAL, DIMENSION(20,20) :: A
REAL, DIMENSION(20) :: b, c

! Inicialización de variables

x = 0.0
y = 0.0
z = 0.0
i = 0
j = 0
k = 0
A = 0.0
b = 0.0
c = 0.0

! Lectura de los datos

OPEN(10, FILE='datos.txt')
READ(10,*) x, y, z

! Cálculo de la matriz A y el vector b

DO i = 1, 20
   DO j = 1, 20
      A(i,j) = 0.0
   END DO
END DO

DO i = 1, 20
   b(i) = 0.0
END DO

DO i = 1, 20
   DO j = 1, 20
      A(i,j) = A(i,j) + x**i * y**j
   END DO
END DO

DO i = 1, 20
   b(i) = b(i) + z**i
END DO

! Resolución del sistema de ecuaciones lineales

CALL DGEFA(A, 20, 20, 1, 1, IPIV)
CALL DGESL(A, 20, 20, IPIV, b, 0)

! Cálculo del vector c

DO i = 1, 20
   c(i) = 0.0
END DO

DO i = 1, 20
   DO j = 1, 20
      c(i) = c(i) + A(i,j) * b(j)
   END DO
END DO

! Impresión de los resultados

OPEN(20, FILE='resultados.txt')
WRITE(20,*) 'Solución del problema de optimización:'
WRITE(20,*) 'x = ', x
WRITE(20,*) 'y = ', y
WRITE(20,*) 'z = ', z

! Fin del programa

END PROGRAMA PRINCIPAL
```

Este código resuelve un problema de optimización no lineal utilizando el método de los mínimos cuadrados.

El problema de optimización consiste en minimizar la función objetivo

```
f(x, y, z) = (z - x^2 - y^2)^2
```

sujeta a las restricciones

```
x + y + z = 1
x^2 + y^2 + z^2 = 1
```

El código comienza por declarar las variables que se utilizarán en el programa. A continuación, se lee el valor de las variables `x`, `y` y `z` de un archivo de texto.

A continuación, se calcula la matriz `A` y el vector `b` del sistema de ecuaciones lineales que se debe resolver para obtener los valores de los coeficientes del polinomio que minimiza la función objetivo.

El sistema de ecuaciones lineales se resuelve utilizando la subrutina `DGEFA` y `DGESL` de la biblioteca LAPACK.

Una vez que se han obtenido los valores de los coeficientes del polinomio, se calcula el valor de la función objetivo en estos valores.

Por último, se imprimen los resultados en un archivo de texto.