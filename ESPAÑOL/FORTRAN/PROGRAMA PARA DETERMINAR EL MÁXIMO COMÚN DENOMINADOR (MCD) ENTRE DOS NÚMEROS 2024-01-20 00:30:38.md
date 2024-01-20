```fortran
PROGRAMA DETERMINAR EL MÁXIMO COMÚN DENOMINADOR (MCD) ENTRE DOS NÚMEROS

IMPLICIT NONE

! DECLARACIÓN DE VARIABLES
INTEGER :: N1, N2, MCD

! SOLICITUD DE LOS NÚMEROS AL USUARIO
WRITE(*,*) 'INTRODUZCA EL PRIMER NÚMERO:'
READ(*,*) N1
WRITE(*,*) 'INTRODUZCA EL SEGUNDO NÚMERO:'
READ(*,*) N2

! CÁLCULO DEL MCD
IF (N1 == N2) THEN
  MCD = N1
ELSEIF (N1 > N2) THEN
  MCD = CalcularMCD(N1, N2)
ELSE
  MCD = CalcularMCD(N2, N1)
END IF

! MOSTRAR EL RESULTADO
WRITE(*,*) 'EL MÁXIMO COMÚN DENOMINADOR ES:', MCD

CONTAINS

! FUNCIÓN PARA CALCULAR EL MCD ENTRE DOS NÚMEROS
INTEGER FUNCTION CalcularMCD(A, B)

  INTEGER, INTENT(IN) :: A, B
  INTEGER :: MCD

  MCD = MOD(A, B)

  WHILE (MCD /= 0) DO
    A = B
    B = MCD
    MCD = MOD(A, B)
  END DO

  CalcularMCD = B

END FUNCTION CalcularMCD

END PROGRAMA
```

Explicación del código:

* El programa comienza declarando las variables que se utilizarán en el código.
* A continuación, se solicita al usuario que introduzca los dos números entre los que se desea calcular el MCD.
* El programa calcula el MCD utilizando una función llamada `CalcularMCD`. Esta función utiliza el algoritmo de Euclides para calcular el MCD.
* Una vez que se ha calculado el MCD, se muestra el resultado al usuario.

A continuación se explica el código de la función `CalcularMCD`:

* La función toma dos números enteros como argumentos de entrada y devuelve el MCD de los dos números.
* La función utiliza un bucle `WHILE` para calcular el MCD.
* El bucle `WHILE` continúa hasta que el resto de la división de `A` entre `B` sea igual a cero.
* En cada iteración del bucle, `A` se convierte en `B` y `B` se convierte en el resto de la división de `A` entre `B`.
* El MCD es el valor final de `B` cuando el bucle `WHILE` termina.