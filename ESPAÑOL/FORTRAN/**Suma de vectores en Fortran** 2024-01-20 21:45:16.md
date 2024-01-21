```fortran
PROGRAMA PRINCIPAL

! Definición de variables
IMPLICIT NONE

INTEGER, PARAMETER :: N = 100 ! Tamaño del vector

REAL, DIMENSION(N) :: A, B, C ! Vectores

! Bucle para inicializar los vectores
DO I = 1, N
    A(I) = 1.0
    B(I) = 2.0
END DO

! Bucle para calcular el vector C
DO I = 1, N
    C(I) = A(I) + B(I)
END DO

! Impresión de los vectores
PRINT *, 'Vector A:'
PRINT *, A

PRINT *, 'Vector B:'
PRINT *, B

PRINT *, 'Vector C:'
PRINT *, C

END PROGRAMA PRINCIPAL
```

Explicación del código:

* El programa principal define las variables necesarias para el programa, incluyendo el tamaño del vector N, los vectores A, B y C, y los parámetros de impresión.
* El bucle DO inicializa los vectores A y B con valores específicos.
* El bucle DO calcula el vector C como la suma de los vectores A y B.
* Finalmente, el programa imprime los vectores A, B y C.

Este código es complejo en el sentido de que utiliza varios bucles DO y parámetros para definir los vectores y calcular su suma. Sin embargo, el código está bien organizado y comentado, lo que lo hace fácil de entender y modificar.