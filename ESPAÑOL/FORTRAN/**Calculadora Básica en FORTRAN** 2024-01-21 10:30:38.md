```fortran
PROGRAMA CALCULADORA
! Esta calculadora realiza operaciones básicas aritméticas (suma, resta, multiplicación y división).

! Declaración de variables
REAL :: numero1, numero2, resultado
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

  ! Realizar la operación correspondiente según la opción elegida
  SELECT CASE (opcion)
    CASE (1)
      ! Suma
      PRINT *, "Ingrese el primer número:"
      READ *, numero1
      PRINT *, "Ingrese el segundo número:"
      READ *, numero2
      resultado = numero1 + numero2
      PRINT *, "El resultado de la suma es:", resultado
    CASE (2)
      ! Resta
      PRINT *, "Ingrese el primer número:"
      READ *, numero1
      PRINT *, "Ingrese el segundo número:"
      READ *, numero2
      resultado = numero1 - numero2
      PRINT *, "El resultado de la resta es:", resultado
    CASE (3)
      ! Multiplicación
      PRINT *, "Ingrese el primer número:"
      READ *, numero1
      PRINT *, "Ingrese el segundo número:"
      READ *, numero2
      resultado = numero1 * numero2
      PRINT *, "El resultado de la multiplicación es:", resultado
    CASE (4)
      ! División
      PRINT *, "Ingrese el primer número:"
      READ *, numero1
      PRINT *, "Ingrese el segundo número:"
      READ *, numero2
      IF (numero2 == 0) THEN
        PRINT *, "No se puede dividir por cero."
      ELSE
        resultado = numero1 / numero2
        PRINT *, "El resultado de la división es:", resultado
      END IF
    CASE (5)
      ! Salir del programa
      EXIT
    CASE DEFAULT
      ! Opción no válida
      PRINT *, "Opción no válida."
  END SELECT

! Repetir el bucle hasta que se elija la opción de salir
END DO

END PROGRAM CALCULADORA
```

**Explicación del código:**

* El programa comienza con la declaración del programa `CALCULADORA`.
* A continuación, se declaran las variables que se utilizarán en el programa:
    * `numero1` y `numero2`: variables reales para almacenar los dos números que se van a operar.
    * `resultado`: variable real para almacenar el resultado de la operación.
    * `opcion`: variable entera para almacenar la opción elegida por el usuario.
* El bucle principal del programa `DO` se ejecuta hasta que el usuario elija la opción de salir.
* Dentro del bucle principal, se muestra el menú de opciones y se lee la opción elegida por el usuario.
* A continuación, se realiza la operación correspondiente según la opción elegida.
* Por último, se repite el bucle principal hasta que el usuario elija la opción de salir.