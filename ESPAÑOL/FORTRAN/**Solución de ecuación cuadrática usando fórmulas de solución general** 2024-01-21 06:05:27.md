```fortran
PROGRAMA PRINCIPAL:

IMPLICIT NONE

! Declaración de variables

REAL :: a, b, c, d

! Entrada de datos

PRINT *, "Introduzca el valor de a:"
READ *, a

PRINT *, "Introduzca el valor de b:"
READ *, b

PRINT *, "Introduzca el valor de c:"
READ *, c

PRINT *, "Introduzca el valor de d:"
READ *, d

! Cálculos

IF (a .EQ. 0.0) THEN
    PRINT *, "El valor de a no puede ser cero."
ELSE
    x = (-b + SQRT(b**2 - 4.0*a*c)) / (2.0*a)
    y = (-b - SQRT(b**2 - 4.0*a*c)) / (2.0*a)
    PRINT *, "Las soluciones son:", x, y
END IF

END PROGRAMA PRINCIPAL
```

Explicación del código:

1. **Declaración de variables**: Se declaran las variables que se utilizarán en el programa. En este caso, se declaran cuatro variables reales `a`, `b`, `c` y `d`.

2. **Entrada de datos**: Se solicita al usuario que introduzca los valores de las variables `a`, `b`, `c` y `d`.

3. **Cálculos**: Se calculan las soluciones de la ecuación cuadrática `a*x^2 + b*x + c = 0`. Para ello, se utilizan las fórmulas de la solución general.

4. **Salida de datos**: Se muestran por pantalla las soluciones de la ecuación cuadrática.

Este código es bastante complejo para los estándares de FORTRAN y es poco probable que se repita en la práctica. Sin embargo, es un buen ejemplo de cómo se pueden utilizar las estructuras de control y las operaciones matemáticas en FORTRAN para resolver problemas complejos.