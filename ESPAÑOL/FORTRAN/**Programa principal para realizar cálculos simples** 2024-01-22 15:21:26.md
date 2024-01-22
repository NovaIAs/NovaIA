```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
REAL :: A, B, C, D
INTEGER :: I, J, K

! Entrada de datos
PRINT *, "Ingrese el valor de A:"
READ *, A
PRINT *, "Ingrese el valor de B:"
READ *, B
PRINT *, "Ingrese el valor de C:"
READ *, C
PRINT *, "Ingrese el valor de D:"
READ *, D

! Cálculos
I = A + B
J = C - D
K = A * B / C + D

! Salida de datos
PRINT *, "El valor de I es:", I
PRINT *, "El valor de J es:", J
PRINT *, "El valor de K es:", K

FIN PROGRAMA PRINCIPAL
```

Explicación del código:

* El programa principal es el punto de entrada del programa.
* Las variables `A`, `B`, `C` y `D` son declaradas como reales, e `I`, `J` y `K` son declaradas como enteras.
* El programa solicita al usuario que ingrese los valores de `A`, `B`, `C` y `D`.
* El programa realiza los cálculos de `I`, `J` y `K`.
* El programa imprime los valores de `I`, `J` y `K`.