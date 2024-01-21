```fortran
PROGRAMA PRINCIPAL
IMPLICIT NONE

VARIABLES
REAL :: X, Y, Z
INTEGER :: I, J, K

INICIO DEL PROGRAMA
WRITE(*,*) '¡Hola, mundo!'

BUCLE DO WHILE
DO WHILE (I.LT.10)
    X = I
    Y = X**2
    Z = X**3
    WRITE(*,*) X, Y, Z
    I = I + 1
END DO

BUCLE FOR
DO I = 1, 10
    X = I
    Y = X**2
    Z = X**3
    WRITE(*,*) X, Y, Z
END DO

BUCLE NESTED
DO I = 1, 10
    DO J = 1, 10
        X = I
        Y = J
        Z = X**2 + Y**2
        WRITE(*,*) X, Y, Z
    END DO
END DO

INSTRUCCIÓN IF
X = 10
IF (X.GT.0) THEN
    WRITE(*,*) 'X es positivo'
ELSE IF (X.LT.0) THEN
    WRITE(*,*) 'X es negativo'
ELSE
    WRITE(*,*) 'X es cero'
END IF

INSTRUCCIÓN SELECT CASE
SELECT CASE (X)
CASE (1)
    WRITE(*,*) 'X es uno'
CASE (2)
    WRITE(*,*) 'X es dos'
CASE (3)
    WRITE(*,*) 'X es tres'
CASE DEFAULT
    WRITE(*,*) 'X no es uno, dos ni tres'
END SELECT

INSTRUCCIÓN DO
DO I = 1, 10
    WRITE(*,*) I
END DO

INSTRUCCIÓN EXIT
DO I = 1, 10
    IF (I.GT.5) THEN
        EXIT
    END IF
    WRITE(*,*) I
END DO

FIN DEL PROGRAMA
END PROGRAMA PRINCIPAL
```

Este código es un programa completo en Fortran que realiza una serie de operaciones matemáticas y muestra los resultados en la pantalla. El programa comienza declarando las variables que se utilizarán en el programa. A continuación, el programa utiliza una serie de instrucciones para realizar las operaciones matemáticas. Las instrucciones utilizadas incluyen bucles, condiciones y selecciones. El programa finaliza mostrando los resultados de las operaciones matemáticas en la pantalla.

A continuación se explica el código paso a paso:

* **Declaración de variables:** Las variables se declaran utilizando la instrucción `IMPLICIT NONE`, que indica que todas las variables deben ser declaradas explícitamente. Las variables utilizadas en el programa son:

    * `X`: Un número real.
    * `Y`: Un número real.
    * `Z`: Un número real.
    * `I`: Un número entero.
    * `J`: Un número entero.
    * `K`: Un número entero.

* **Inicio del programa:** El programa comienza con la instrucción `PROGRAMA PRINCIPAL`, que indica el inicio del programa.

* **Bucle DO WHILE:** El bucle `DO WHILE` se utiliza para repetir una serie de instrucciones mientras una condición sea verdadera. En este caso, el bucle se repetirá mientras `I` sea menor que 10. Dentro del bucle, el programa calcula los valores de `X`, `Y` y `Z` y los muestra en la pantalla.

* **Bucle FOR:** El bucle `FOR` se utiliza para repetir una serie de instrucciones un número determinado de veces. En este caso, el bucle se repetirá 10 veces. Dentro del bucle, el programa calcula los valores de `X`, `Y` y `Z` y los muestra en la pantalla.

* **Bucle NESTED:** El bucle `NESTED` es un bucle que se encuentra dentro de otro bucle. En este caso, el bucle `NESTED` se encuentra dentro del bucle `FOR`. Dentro del bucle `NESTED`, el programa calcula los valores de `X`, `Y` y `Z` y los muestra en la pantalla.

* **Instrucción IF:** La instrucción `IF` se utiliza para comprobar si una condición es verdadera. Si la condición es verdadera, se ejecuta la instrucción `THEN`. Si la condición es falsa, se ejecuta la instrucción `ELSE`. En este caso, la instrucción `IF` se utiliza para comprobar si `X` es mayor que 0, menor que 0 o igual a 0. Si `X` es mayor que 0, se muestra el mensaje "X es positivo". Si `X` es menor que 0, se muestra el mensaje "X es negativo". Si `X` es igual a 0, se muestra el mensaje "X es cero".

* **Instrucción SELECT CASE:** La instrucción `SELECT CASE` se utiliza para seleccionar una de varias instrucciones en función del valor de una variable. En este caso, la instrucción `SELECT CASE` se utiliza para seleccionar una instrucción en función del valor de `X`. Si `X` es igual a 1, se muestra el mensaje "X es uno". Si `X` es igual a 2, se muestra el mensaje "X es dos". Si `X` es igual a 3, se muestra el mensaje "X es tres". Si `X` no es igual a 1, 2 ni 3, se muestra el mensaje "X no es uno, dos ni tres".

* **Instrucción DO:** La instrucción `DO` se utiliza para repetir una serie de instrucciones un número determinado de veces. En este caso, la instrucción `DO` se utiliza para repetir la instrucción `WRITE` 10 veces. La instrucción `WRITE` se utiliza para mostrar un mensaje en la pantalla.

* **Instrucción EXIT:** La instrucción `EXIT` se utiliza para salir de un bucle. En este caso, la instrucción `EXIT` se utiliza para salir del bucle `DO` cuando `I` es mayor que 5.

* **Fin del programa:** El programa finaliza con la instrucción `END PROGRAMA PRINCIPAL`, que indica el final del programa.