```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
IMPLICIT NONE
INTEGER :: i, j, k
REAL :: x, y, z

! Bucle anidado
DO i = 1, 10
    DO j = 1, 20
        DO k = 1, 30
            x = i + j + k
            y = x * 2.0
            z = y / 3.0
        END DO
    END DO
END DO

! Impresión de resultados
PRINT *, 'x = ', x
PRINT *, 'y = ', y
PRINT *, 'z = ', z

END PROGRAMA PRINCIPAL
```

Explicación:

* El programa principal se define mediante la primera línea `PROGRAMA PRINCIPAL`.
* La segunda línea `IMPLICIT NONE` indica que todas las variables deben ser declaradas explícitamente.
* Las variables `i`, `j` y `k` se declaran como enteros (`INTEGER`) mediante la línea `INTEGER :: i, j, k`.
* Las variables `x`, `y` y `z` se declaran como reales (`REAL`) mediante la línea `REAL :: x, y, z`.
* El bucle anidado se define mediante las líneas `DO i = 1, 10`, `DO j = 1, 20` y `DO k = 1, 30`.
* Dentro del bucle anidado, se calculan los valores de `x`, `y` y `z`.
* Los resultados se imprimen en la consola mediante las líneas `PRINT *, 'x = ', x`, `PRINT *, 'y = ', y` y `PRINT *, 'z = ', z`.
* El programa finaliza con la línea `END PROGRAMA PRINCIPAL`.