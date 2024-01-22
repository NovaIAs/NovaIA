```fortran
PROGRAMA CALCULADORA

! Declaración de variables
REAL :: a, b, resultado
INTEGER :: opcion

! Bucle principal del programa
DO
  ! Mostrar el menú de opciones
  PRINT *, "CALCULADORA"
  PRINT *, "1. Suma"
  PRINT *, "2. Resta"
  PRINT *, "3. Multiplicación"
  PRINT *, "4. División"
  PRINT *, "5. Salir"

  ! Leer la opción elegida
  READ *, opcion

  ! Realizar la operación correspondiente
  SELECT CASE (opcion)
    CASE (1)
      PRINT *, "Introduzca el primer número:"
      READ *, a
      PRINT *, "Introduzca el segundo número:"
      READ *, b
      resultado = a + b
      PRINT *, "El resultado es:", resultado
    CASE (2)
      PRINT *, "Introduzca el primer número:"
      READ *, a
      PRINT *, "Introduzca el segundo número:"
      READ *, b
      resultado = a - b
      PRINT *, "El resultado es:", resultado
    CASE (3)
      PRINT *, "Introduzca el primer número:"
      READ *, a
      PRINT *, "Introduzca el segundo número:"
      READ *, b
      resultado = a * b
      PRINT *, "El resultado es:", resultado
    CASE (4)
      PRINT *, "Introduzca el primer número:"
      READ *, a
      PRINT *, "Introduzca el segundo número:"
      READ *, b
      IF (b /= 0) THEN
        resultado = a / b
        PRINT *, "El resultado es:", resultado
      ELSE
        PRINT *, "Error: no se puede dividir entre 0"
      END IF
    CASE (5)
      EXIT
    CASE DEFAULT
      PRINT *, "Opción no válida"
  END SELECT

! Fin del bucle principal
END DO

END PROGRAM CALCULADORA
```

Explicación del código:

* La primera línea del código es una declaración del programa, que indica que se trata de un programa de FORTRAN.
* Las líneas siguientes declaran las variables que se utilizarán en el programa. En este caso, las variables son `a`, `b` y `resultado`, que son de tipo real (números de coma flotante), y `opcion`, que es de tipo entero.
* El bucle `DO` principal del programa se ejecuta hasta que el usuario elige la opción 5 (Salir).
* Dentro del bucle, se muestra el menú de opciones, se lee la opción elegida por el usuario y se realiza la operación correspondiente.
* Las opciones 1, 2, 3 y 4 realizan las operaciones de suma, resta, multiplicación y división, respectivamente.
* La opción 5 sale del programa.
* La opción por defecto imprime un mensaje de error si el usuario elige una opción no válida.
* El bucle `END DO` cierra el bucle principal del programa.
* La línea `END PROGRAM CALCULADORA` finaliza el programa.