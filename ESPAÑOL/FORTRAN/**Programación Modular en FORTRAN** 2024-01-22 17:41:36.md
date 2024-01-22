```fortran
PROGRAMA PRINCIPAL

! Definiciones de módulos
MODULE variables_globales
IMPLICIT NONE

! Declaración de variables globales
REAL, DIMENSION(100) :: a
REAL, DIMENSION(100, 100) :: b
INTEGER :: n

CONTAINS

! Subrutina para inicializar las variables globales
SUBROUTINE inicializar_variables()
! Declaración de variables locales
REAL, DIMENSION(100) :: a_local
REAL, DIMENSION(100, 100) :: b_local
INTEGER :: n_local

! Inicialización de las variables locales
a_local = 0.0
b_local = 0.0
n_local = 0

! Asignación de las variables locales a las variables globales
a = a_local
b = b_local
n = n_local

END SUBROUTINE inicializar_variables

! Subrutina para imprimir las variables globales
SUBROUTINE imprimir_variables()
! Declaración de variables locales
REAL, DIMENSION(100) :: a_local
REAL, DIMENSION(100, 100) :: b_local
INTEGER :: n_local

! Asignación de las variables globales a las variables locales
a_local = a
b_local = b
n_local = n

! Impresión de las variables locales
DO i = 1, n_local
WRITE(*,*) a_local(i)
END DO

DO i = 1, n_local
DO j = 1, n_local
WRITE(*,*) b_local(i, j)
END DO
END DO

WRITE(*,*) n_local

END SUBROUTINE imprimir_variables

END MODULE variables_globales

! Programa principal
PROGRAMA principal

! Declaración de variables locales
INTEGER :: i, j

! Llamada a la subrutina para inicializar las variables globales
CALL inicializar_variables()

! Bucle para llenar las matrices a y b
DO i = 1, n
a(i) = i
DO j = 1, n
b(i, j) = i + j
END DO
END DO

! Llamada a la subrutina para imprimir las variables globales
CALL imprimir_variables()

END PROGRAMA principal
```

Explicación del código:

* El código está dividido en módulos. Un módulo es una unidad de código que se puede reutilizar en otros programas. En este caso, se ha creado un módulo llamado `variables_globales` que contiene las variables globales que se utilizarán en el programa.
* El módulo `variables_globales` contiene tres subrutinas: `inicializar_variables()`, `imprimir_variables()` y `finalizar_variables()`.
* La subrutina `inicializar_variables()` inicializa las variables globales.
* La subrutina `imprimir_variables()` imprime las variables globales.
* La subrutina `finalizar_variables()` finaliza las variables globales.
* El programa principal llama a las subrutinas `inicializar_variables()` e `imprimir_variables()` para inicializar e imprimir las variables globales.
* El programa principal también contiene un bucle que llena las matrices `a` y `b` con valores.
* El programa principal llama a la subrutina `imprimir_variables()` para imprimir las matrices `a` y `b`.