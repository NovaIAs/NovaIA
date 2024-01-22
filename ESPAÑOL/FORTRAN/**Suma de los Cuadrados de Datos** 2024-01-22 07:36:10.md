```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
INTEGER :: i, j, k, n
REAL :: x(100), y(100), z(100)

! Lectura de datos
PRINT *, "Introduzca el número de datos:"
READ *, n

DO i = 1, n
  PRINT *, "Introduzca el valor de x(", i, "):"
  READ *, x(i)
  PRINT *, "Introduzca el valor de y(", i, "):"
  READ *, y(i)
END DO

! Cálculo de la suma de los cuadrados de los datos
DO i = 1, n
  z(i) = x(i)**2 + y(i)**2
END DO

! Impresión de los resultados
PRINT *, "Los valores de z son:"
DO i = 1, n
  PRINT *, z(i)
END DO

END PROGRAMA PRINCIPAL
```

Explicación del código:

* El programa principal comienza con la declaración de variables, que son todas de tipo entero o real.
* A continuación, se lee el número de datos que el usuario desea introducir.
* Se crea un bucle que itera desde 1 hasta n.
* Dentro del bucle, se le solicita al usuario que introduzca los valores de x e y para cada dato.
* Una vez que se han introducido todos los datos, se calcula la suma de los cuadrados de los datos.
* Por último, se imprimen los resultados.