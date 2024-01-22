```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
INTEGER :: i, j, k, l
REAL :: x, y, z
CHARACTER(len=10) :: nombre

! Bucle anidado
DO i = 1, 10
  DO j = 1, 10
    DO k = 1, 10
      DO l = 1, 10
        x = x + y * z
      END DO
    END DO
  END DO
END DO

! Sentencia IF
IF (x > 0) THEN
  y = y + 1
ELSE
  z = z + 1
END IF

! Sentencia SELECT CASE
SELECT CASE (l)
  CASE (1)
    nombre = 'Uno'
  CASE (2)
    nombre = 'Dos'
  CASE (3)
    nombre = 'Tres'
  ELSE
    nombre = 'Otro'
END SELECT

! Sentencia DO WHILE
DO WHILE (x < 10)
  x = x + 1
END DO

! Sentencia DO UNTIL
DO UNTIL (y > 10)
  y = y + 1
END DO

! Sentencia EXIT
DO i = 1, 10
  IF (i == 5) EXIT
  x = x + 1
END DO

! Sentencia CONTINUE
DO i = 1, 10
  IF (i == 5) CONTINUE
  x = x + 1
END DO

! Sentencia STOP
STOP

END PROGRAMA PRINCIPAL
```

Explicación del código:

* El programa comienza con la declaración de variables.
* A continuación, hay un bucle anidado que itera sobre cuatro índices.
* Dentro del bucle anidado, se realiza una operación matemática que suma el producto de dos variables a una tercera variable.
* Después del bucle anidado, hay una sentencia IF que comprueba si el valor de la variable `x` es mayor que 0. Si es así, se incrementa el valor de la variable `y`, de lo contrario, se incrementa el valor de la variable `z`.
* A continuación, hay una sentencia SELECT CASE que comprueba el valor de la variable `l`. En función del valor de `l`, se asigna un valor diferente a la variable `nombre`.
* Después de la sentencia SELECT CASE, hay una sentencia DO WHILE que itera mientras el valor de la variable `x` sea menor que 10. Dentro del bucle, se incrementa el valor de la variable `x`.
* A continuación, hay una sentencia DO UNTIL que itera hasta que el valor de la variable `y` sea mayor que 10. Dentro del bucle, se incrementa el valor de la variable `y`.
* Después de la sentencia DO UNTIL, hay una sentencia EXIT que sale del bucle DO actual.
* A continuación, hay una sentencia CONTINUE que continúa con la siguiente iteración del bucle DO actual.
* Por último, el programa termina con una sentencia STOP.