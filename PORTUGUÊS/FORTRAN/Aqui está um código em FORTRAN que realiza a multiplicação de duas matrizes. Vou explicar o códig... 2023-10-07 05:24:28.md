Claro! Aqui está um código em FORTRAN que realiza a multiplicação de duas matrizes. Vou explicar o código para você:

```
PROGRAM multiplicacao_matrizes
    IMPLICIT NONE

    INTEGER, PARAMETER :: n = 3
    INTEGER :: i, j, k
    REAL :: matrizA(n, n), matrizB(n, n), matrizC(n, n)

    ! Inicializando as matrizes A e B
    matrizA = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0], [n, n])
    matrizB = reshape([9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0], [n, n])

    ! Multiplicação das matrizes A e B
    DO i = 1, n
        DO j = 1, n
            matrizC(i, j) = 0.0
            DO k = 1, n
                matrizC(i, j) = matrizC(i, j) + matrizA(i, k) * matrizB(k, j)
            END DO
        END DO
    END DO

    ! Imprimindo a matriz resultante C
    PRINT *, "Matriz C:"
    DO i = 1, n
        DO j = 1, n
            PRINT *, matrizC(i, j)
        END DO
    END DO

END PROGRAM multiplicacao_matrizes
```

Neste código, estamos calculando a multiplicação de duas matrizes quadradas de tamanho `n` (definido como 3 neste exemplo). As matrizes A e B são inicializadas com valores específicos.

Em seguida, usamos três loops DO aninhados para calcular o elemento `(i, j)` da matriz resultante C. O primeiro loop percorre as linhas da matriz A, o segundo loop percorre as colunas da matriz B e o terceiro loop calcula a multiplicação dos elementos correspondentes e soma-os ao elemento `(i, j)` da matriz C.

Por fim, imprimimos a matriz resultante C na tela.