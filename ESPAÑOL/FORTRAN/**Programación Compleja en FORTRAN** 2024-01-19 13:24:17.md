```fortran
PROGRAM CODIGO_COMPLEJO_FORTRAN

IMPLICIT NONE

! DECLARACIÓN DE VARIABLES
INTEGER :: ENTERO = 10
REAL :: REAL = 3.14
CHARACTER(LEN=20) :: CADENA = "Hola Mundo"
LOGICAL :: LOGICO = .TRUE.

! DECLARACIÓN DE SUBRUTINA
SUBROUTINE MI_SUBRUTINA(A, B, C)

IMPLICIT NONE

INTEGER, INTENT(IN) :: A
REAL, INTENT(IN) :: B
CHARACTER(LEN=20), INTENT(IN) :: C

! CUERPO DE LA SUBRUTINA
PRINT *, "A =", A
PRINT *, "B =", B
PRINT *, "C =", C

END SUBROUTINE MI_SUBRUTINA

! CUERPO PRINCIPAL DEL PROGRAMA
CALL MI_SUBRUTINA(ENTERO, REAL, CADENA)

END PROGRAM CODIGO_COMPLEJO_FORTRAN
```

**Explicación del código:**

- El programa comienza con la declaración de variables, donde se declaran cuatro variables:

  - `ENTERO` es una variable entera con un valor inicial de 10.
  - `REAL` es una variable real con un valor inicial de 3.14.
  - `CADENA` es una variable de carácter con una longitud de 20 caracteres y un valor inicial de "Hola Mundo".
  - `LOGICO` es una variable lógica con un valor inicial de .TRUE..

- A continuación, se declara una subrutina llamada `MI_SUBRUTINA`, que recibe tres argumentos:

  - `A` es un argumento entero.
  - `B` es un argumento real.
  - `C` es un argumento de carácter con una longitud de 20 caracteres.

- El cuerpo de la subrutina imprime los valores de los argumentos recibidos en la consola.

- Finalmente, el programa llama a la subrutina `MI_SUBRUTINA` pasándole los valores de las variables `ENTERO`, `REAL` y `CADENA`, y luego termina.

**Salida del programa:**

```
A = 10
B = 3.14
C = Hola Mundo
```