```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
REAL :: x, y, z
INTEGER :: i, j, k

! Entrada de datos
PRINT *, "Introduzca el valor de x:"
READ *, x
PRINT *, "Introduzca el valor de y:"
READ *, y
PRINT *, "Introduzca el valor de z:"
READ *, z

! Cálculo de la expresión
i = INT(x)
j = INT(y)
k = INT(z)
w = x + y + z
v = i + j + k

! Salida de resultados
PRINT *, "El resultado de la expresión es:", w
PRINT *, "El resultado de la suma de los enteros es:", v

END PROGRAMA PRINCIPAL
```

Explicación del código:

* El programa comienza con la declaración del programa principal, que es el punto de entrada del programa.
* A continuación, se declaran las variables que se van a utilizar en el programa. En este caso, se declaran tres variables reales (x, y y z) y tres variables enteras (i, j y k).
* Se utiliza el comando PRINT para imprimir un mensaje en la consola solicitando al usuario que introduzca los valores de x, y y z.
* Se utiliza el comando READ para leer los valores introducidos por el usuario y almacenarlos en las variables x, y y z.
* Se calculan los valores de i, j y k, que son las partes enteras de x, y y z, respectivamente.
* Se calcula el valor de w, que es la suma de x, y y z.
* Se calcula el valor de v, que es la suma de i, j y k.
* Se utilizan los comandos PRINT para imprimir los resultados del cálculo en la consola.
* El programa termina con la instrucción END PROGRAMA PRINCIPAL.