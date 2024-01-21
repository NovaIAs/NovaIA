```fortran
PROGRAMA PRINCIPAL

IMPLICITO NINGUNO

! DECLARACIÓN DE VARIABLES
REAL, DIMENSION(100) :: A, B, C
INTEGER :: I, J, K

! INICIALIZACIÓN DE VARIABLES
DO I = 1, 100
    A(I) = REAL(I)
    B(I) = REAL(I * 2)
    C(I) = REAL(I * 3)
END DO

! BUCLE PARA CALCULAR LA SUMA DE LOS TRES VECTORES
DO I = 1, 100
    C(I) = A(I) + B(I) + C(I)
END DO

! BUCLE ANIDADO PARA IMPRIMIR LOS RESULTADOS
DO I = 1, 10
    DO J = 1, 10
        DO K = 1, 10
            PRINT *, 'A(', I, ',', J, ',', K, ') = ', A(I * 100 + J * 10 + K)
            PRINT *, 'B(', I, ',', J, ',', K, ') = ', B(I * 100 + J * 10 + K)
            PRINT *, 'C(', I, ',', J, ',', K, ') = ', C(I * 100 + J * 10 + K)
        END DO
        PRINT *, ''
    END DO
    PRINT *, ''
END DO

END PROGRAMA PRINCIPAL
```

Este código es un programa complejo en FORTRAN que calcula la suma de tres vectores de 100 elementos cada uno. El programa también imprime los resultados en un formato tabular.

El código comienza con las declaraciones de las variables. Las variables `A`, `B` y `C` son vectores de 100 elementos de tipo `REAL`. Las variables `I`, `J` y `K` son variables enteras.

La siguiente sección del código inicializa las variables. El vector `A` se inicializa con los valores de 1 a 100. El vector `B` se inicializa con los valores de 2 a 200. El vector `C` se inicializa con los valores de 3 a 300.

El siguiente bucle calcula la suma de los tres vectores. El bucle itera sobre cada elemento de los vectores y suma los valores correspondientes. El resultado se almacena en el vector `C`.

El siguiente bucle anidado imprime los resultados en un formato tabular. El bucle externo itera sobre los índices de las filas de la tabla. El bucle interno itera sobre los índices de las columnas de la tabla. El valor de cada elemento de la tabla se imprime usando la función `PRINT *`.

Por último, el código finaliza con la declaración `END PROGRAMA PRINCIPAL`.