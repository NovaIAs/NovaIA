```fortran
PROGRAMA PRINCIPAL

IMPLICIT NONE

! Declaración de variables
INTEGER :: i, j, k, l, m, n
REAL :: x, y, z
CHARACTER(len=20) :: nombre_usuario

! Pedir al usuario que introduzca su nombre
WRITE(*,*) '¿Cómo te llamas?'
READ(*,*) nombre_usuario

! Mostrar un mensaje de bienvenida al usuario
WRITE(*,*) 'Hola, ', nombre_usuario, '!'

! Crear un bucle for anidado
DO i = 1, 10
  DO j = 1, 10
    DO k = 1, 10
      DO l = 1, 10
        DO m = 1, 10
          DO n = 1, 10
            ! Calcular el valor de x, y y z
            x = i + j + k + l + m + n
            y = i * j * k * l * m * n
            z = (i + j + k + l + m + n) / (i * j * k * l * m * n)

            ! Mostrar los valores de x, y y z
            WRITE(*,*) 'x = ', x
            WRITE(*,*) 'y = ', y
            WRITE(*,*) 'z = ', z
          END DO
        END DO
      END DO
    END DO
  END DO
END DO

! Fin del programa
END PROGRAM PRINCIPAL
```

Explicación del código:

* La primera línea es una declaración que indica que el programa está escrito en FORTRAN.
* La segunda línea es un comentario que indica que todas las variables se declararán explícitamente.
* Las siguientes líneas declaran las variables que se utilizarán en el programa.
* La siguiente línea es una llamada a la función `WRITE` que muestra un mensaje en la pantalla pidiendo al usuario que introduzca su nombre.
* La siguiente línea es una llamada a la función `READ` que lee el nombre del usuario desde la entrada estándar.
* La siguiente línea es una llamada a la función `WRITE` que muestra un mensaje de bienvenida al usuario.
* El siguiente bloque es un bucle `for` anidado que se ejecuta 10 veces para cada variable `i`, `j`, `k`, `l`, `m` y `n`.
* Dentro de cada bucle, se calcula el valor de `x`, `y` y `z` utilizando las operaciones aritméticas básicas.
* A continuación, se muestran los valores de `x`, `y` y `z` en la pantalla.
* El programa finaliza con una llamada a la función `END PROGRAM`, que indica que el programa ha terminado de ejecutarse.