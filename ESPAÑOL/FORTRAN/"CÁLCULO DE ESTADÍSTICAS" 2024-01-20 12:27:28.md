```fortran
PROGRAMA CALCULO_ESTADISTICAS

! Declaración de tipos de datos
TYPE ESTADISTICAS
  REAL :: media
  REAL :: varianza
  REAL :: desviacion_estandar
END TYPE ESTADISTICAS

! Declaración de subrutinas
SUBROUTINE LEER_DATOS(datos, n)
  ! Lee los datos de entrada y los almacena en el vector datos.
  ! Parámetro de entrada:
  !   datos: vector donde se almacenarán los datos.
  !   n: número de datos a leer.

  DO i = 1, n
    READ *, datos(i)
  END DO

END SUBROUTINE LEER_DATOS

SUBROUTINE CALCULAR_ESTADISTICAS(datos, n, estadisticas)
  ! Calcula las estadísticas de los datos.
  ! Parámetros de entrada:
  !   datos: vector con los datos.
  !   n: número de datos.
  ! Parámetro de salida:
  !   estadisticas: estructura con las estadísticas calculadas.

  ! Calcula la media.
  estadisticas%media = SUM(datos) / n

  ! Calcula la varianza.
  varianza = SUM((datos - estadisticas%media)**2) / (n - 1)

  ! Calcula la desviación estándar.
  estadisticas%desviacion_estandar = SQRT(varianza)

END SUBROUTINE CALCULAR_ESTADISTICAS

SUBROUTINE IMPRIMIR_ESTADISTICAS(estadisticas)
  ! Imprime las estadísticas.
  ! Parámetro de entrada:
  !   estadisticas: estructura con las estadísticas calculadas.

  PRINT *, "Media:", estadisticas%media
  PRINT *, "Varianza:", estadisticas%varianza
  PRINT *, "Desviación estándar:", estadisticas%desviacion_estandar

END SUBROUTINE IMPRIMIR_ESTADISTICAS

! Programa principal
IMPLICIT NONE

! Declaración de variables
INTEGER :: n
REAL, DIMENSION(100) :: datos
TYPE(ESTADISTICAS) :: estadisticas

! Lee los datos de entrada.
PRINT *, "Introduce el número de datos:"
READ *, n

! Verifica que el número de datos sea válido.
IF (n <= 0) THEN
  PRINT *, "El número de datos debe ser positivo."
  STOP
END IF

! Lee los datos.
CALL LEER_DATOS(datos, n)

! Calcula las estadísticas.
CALL CALCULAR_ESTADISTICAS(datos, n, estadisticas)

! Imprime las estadísticas.
CALL IMPRIMIR_ESTADISTICAS(estadisticas)

END PROGRAMA CALCULO_ESTADISTICAS
```

Este código calcula las estadísticas básicas de un conjunto de datos. Las estadísticas calculadas son la media, la varianza y la desviación estándar. El código está organizado en tres subrutinas y un programa principal.

La subrutina `LEER_DATOS` lee los datos de entrada y los almacena en un vector. La subrutina `CALCULAR_ESTADISTICAS` calcula las estadísticas de los datos. La subrutina `IMPRIMIR_ESTADISTICAS` imprime las estadísticas. El programa principal llama a las subrutinas para leer los datos, calcular las estadísticas e imprimirlas.