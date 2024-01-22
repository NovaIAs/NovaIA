```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
INTEGER :: i, j, k, l, m, n
REAL :: x, y, z, a, b, c
CHARACTER(len=20) :: nombre, apellido

! Inicialización de variables
i = 1
j = 2
k = 3
l = 4
m = 5
n = 6
x = 1.0
y = 2.0
z = 3.0
a = 4.0
b = 5.0
c = 6.0
nombre = "Juan"
apellido = "Pérez"

! Bucle FOR
DO i = 1, 10
    PRINT *, "El valor de i es:", i
END DO

! Bucle DO WHILE
DO WHILE (i < 20)
    PRINT *, "El valor de i es:", i
    i = i + 1
END DO

! Bucle DO UNTIL
DO UNTIL (i >= 30)
    PRINT *, "El valor de i es:", i
    i = i + 1
END DO

! Sentencia IF
IF (i > 30) THEN
    PRINT *, "El valor de i es mayor que 30"
ELSEIF (i == 30) THEN
    PRINT *, "El valor de i es igual a 30"
ELSE
    PRINT *, "El valor de i es menor que 30"
END IF

! Sentencia SELECT CASE
SELECT CASE (i)
    CASE (1)
        PRINT *, "El valor de i es 1"
    CASE (2)
        PRINT *, "El valor de i es 2"
    CASE (3)
        PRINT *, "El valor de i es 3"
    CASE DEFAULT
        PRINT *, "El valor de i no es 1, 2 ni 3"
END SELECT

! Salida de datos
PRINT *, "El nombre es:", nombre
PRINT *, "El apellido es:", apellido
PRINT *, "El valor de x es:", x
PRINT *, "El valor de y es:", y
PRINT *, "El valor de z es:", z
PRINT *, "El valor de a es:", a
PRINT *, "El valor de b es:", b
PRINT *, "El valor de c es:", c

END PROGRAMA PRINCIPAL
```

Este código es un programa complejo en FORTRAN que realiza una serie de operaciones, incluyendo bucles, sentencias condicionales y operaciones de entrada y salida.

El programa comienza declarando una serie de variables, incluyendo variables enteras, variables reales y variables de carácter.

A continuación, el programa inicializa las variables con valores específicos.

El programa entonces entra en un bucle FOR que se ejecuta 10 veces. Dentro del bucle, el programa imprime el valor de la variable i.

Después de que el bucle FOR haya terminado, el programa entra en un bucle DO WHILE que se ejecuta mientras la variable i sea menor que 20. Dentro del bucle, el programa imprime el valor de la variable i y luego incrementa el valor de i en 1.

Una vez que la variable i alcanza el valor 20, el programa sale del bucle DO WHILE y entra en un bucle DO UNTIL que se ejecuta hasta que la variable i sea mayor o igual que 30. Dentro del bucle, el programa imprime el valor de la variable i y luego incrementa el valor de i en 1.

Una vez que la variable i alcanza el valor 30, el programa sale del bucle DO UNTIL y entra en una sentencia IF. La sentencia IF comprueba si la variable i es mayor que 30, si es igual a 30 o si es menor que 30. Si la variable i es mayor que 30, el programa imprime un mensaje indicando que el valor de i es mayor que 30. Si la variable i es igual a 30, el programa imprime un mensaje indicando que el valor de i es igual a 30. Si la variable i es menor que 30, el programa imprime un mensaje indicando que el valor de i es menor que 30.

Después de la sentencia IF, el programa entra en una sentencia SELECT CASE. La sentencia SELECT CASE comprueba el valor de la variable i y ejecuta un código diferente dependiendo del valor de i. Si el valor de i es 1, el programa imprime un mensaje indicando que el valor de i es 1. Si el valor de i es 2, el programa imprime un mensaje indicando que el valor de i es 2. Si el valor de i es 3, el programa imprime un mensaje indicando que el valor de i es 3. Si el valor de i no es 1, 2 o 3, el programa imprime un mensaje indicando que el valor de i no es 1, 2 ni 3.

Por último, el programa imprime el valor de las variables nombre, apellido, x, y, z, a, b y c.