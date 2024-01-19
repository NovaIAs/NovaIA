```fortran
PROGRAM PruebaFORTRAN
! Este programa calcula el área de un triángulo, utilizando la fórmula:
! Área = (base * altura) / 2

! Declaración de variables
REAL :: base, altura, area  ! Variables de tipo real (números decimales)

! Entrada de datos
PRINT *, "Escribe la base del triángulo:"
READ *, base
PRINT *, "Escribe la altura del triángulo:"
READ *, altura

! Cálculo del área
area = 0.5 * base * altura

! Salida de datos
PRINT *, "El área del triángulo es:", area

END PROGRAM PruebaFORTRAN
```

Este código es un poco más complejo que el anterior, ya que utiliza diferentes tipos de datos y realiza un cálculo más complejo.

* Las variables `base`, `altura` y `area` son de tipo `REAL`, lo que significa que pueden almacenar números decimales.
* El cálculo del área se realiza mediante la fórmula `area = 0.5 * base * altura`.
* La salida de datos se realiza mediante la instrucción `PRINT`, que imprime el valor de `area` en la consola.

Este código es un ejemplo sencillo de cómo utilizar FORTRAN para realizar cálculos matemáticos.