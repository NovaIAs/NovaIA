```fortran
PROGRAMA FACTORIAL

! Declaración de variables
INTEGER :: numero, factorial

! Bucle para pedir el número al usuario y comprobar que es positivo
DO
  WRITE (*, '(A)') 'Introduce un número positivo:'
  READ (*, *) numero
  IF (numero < 0) THEN
    WRITE (*, '(A)') 'El número debe ser positivo.'
  END IF
END DO

! Cálculo del factorial
factorial = 1
DO i = 1, numero
  factorial = factorial * i
END DO

! Impresión del resultado
WRITE (*, '(A, I0, A, I0)') 'El factorial de ', numero, ' es ', factorial

END PROGRAMA FACTORIAL
```

Explicación del código:

* La primera línea del programa es la declaración del programa, que indica el nombre del programa y su tipo (en este caso, un programa principal).
* Las siguientes líneas son declaraciones de variables, que indican el tipo y el nombre de las variables que se utilizarán en el programa.
* El siguiente bloque de código es un bucle `DO` que se repite hasta que el usuario introduce un número positivo.
* El siguiente bloque de código calcula el factorial del número utilizando otro bucle `DO`.
* La última línea del programa imprime el resultado en la consola.