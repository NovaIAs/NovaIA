```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
REAL:: pi = 3.1416
INTEGER:: n = 100
REAL, DIMENSION(n):: x, y

! Cálculo de los valores de x e y
DO i = 1, n
   x(i) = i / 10.0
   y(i) = SIN(pi * x(i))
END DO

! Gráfica de los datos
CALL GRAFICA(x, y, n)

END PROGRAMA PRINCIPAL

! Subrutina para gráficar los datos
SUBROUTINE GRAFICA(x, y, n)

! Declaración de variables
REAL, DIMENSION(n):: x, y
INTEGER:: n

! Apertura del archivo de salida
OPEN(UNIT=10, FILE='grafica.txt')

! Escritura de los datos en el archivo
DO i = 1, n
   WRITE(10, '(F10.4, F10.4)') x(i), y(i)
END DO

! Cierre del archivo de salida
CLOSE(UNIT=10)

END SUBROUTINE GRAFICA
```

Explicación del código:

1. Programa principal:

   - Se importan las librerías necesarias (por ejemplo, math.f90 para la función SIN).
   - Se declaran las variables necesarias, incluyendo un array para almacenar los valores de x e y.
   - Se calculan los valores de x e y utilizando un bucle DO.
   - Se llama a la subrutina GRAFICA para graficar los datos.

2. Subrutina GRAFICA:

   - Se importan las librerías necesarias (por ejemplo, draw.f90 para las funciones de dibujo).
   - Se declaran las variables necesarias, incluyendo un array para almacenar los valores de x e y.
   - Se abre un archivo de texto para almacenar los datos gráficos.
   - Se escriben los datos en el archivo de texto utilizando un bucle DO.
   - Se cierra el archivo de texto.

3. Ejecución del programa:

   - El programa principal se ejecuta primero.
   - El programa principal llama a la subrutina GRAFICA para graficar los datos.
   - La subrutina GRAFICA escribe los datos en un archivo de texto.
   - El usuario puede abrir el archivo de texto para ver la gráfica.