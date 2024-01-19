```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
IMPLICIT NONE

! Variables de entrada
REAL :: a, b, c

! Variables de salida
REAL :: x1, x2

! Lectura de los coeficientes de la ecuación cuadrática
PRINT *, "Introduce el coeficiente a:"
READ *, a
PRINT *, "Introduce el coeficiente b:"
READ *, b
PRINT *, "Introduce el coeficiente c:"
READ *, c

! Cálculo de las raíces de la ecuación cuadrática
x1 = (-b + SQRT(b**2 - 4*a*c)) / (2*a)
x2 = (-b - SQRT(b**2 - 4*a*c)) / (2*a)

! Impresión de las raíces de la ecuación cuadrática
PRINT *, "Las raíces de la ecuación cuadrática son:"
PRINT *, "x1 =", x1
PRINT *, "x2 =", x2

END PROGRAMA PRINCIPAL
```

Explicación del código:

* El programa principal comienza con la declaración IMPLICIT NONE, que indica al compilador que se deben declarar todas las variables utilizadas en el programa. Esto ayuda a evitar errores de programación causados por el uso de variables no declaradas.
* A continuación, se declaran las variables de entrada y salida del programa. Las variables de entrada son a, b y c, que son los coeficientes de la ecuación cuadrática. Las variables de salida son x1 y x2, que son las raíces de la ecuación cuadrática.
* El programa utiliza la función READ para leer los valores de las variables de entrada.
* El programa calcula las raíces de la ecuación cuadrática utilizando la fórmula cuadrática, que se implementa en las líneas 17 y 18.
* El programa utiliza la función PRINT para imprimir los valores de las raíces de la ecuación cuadrática.
* El programa termina con la línea END PROGRAMA PRINCIPAL.