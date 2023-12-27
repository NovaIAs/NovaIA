Claro! Aqui está um código em FORTRAN que realiza a análise de dados em um conjunto de amostras de uma pesquisa de opinião. O código solicitará ao usuário que insira o número de amostras e, em seguida, solicitará as respostas para cada amostra. Em seguida, ele calculará a média, a moda e a mediana das respostas.

```
PROGRAM AnaliseDeDados
    IMPLICIT NONE
    
    INTEGER :: numAmostras, i, j
    INTEGER, DIMENSION(:), ALLOCATABLE :: amostras
    INTEGER :: resposta, soma, media, moda, mediana
    
    ! Solicita ao usuário o número de amostras
    WRITE(*,*) 'Insira o número de amostras:'
    READ(*,*) numAmostras
    
    ! Aloca memória para o array de amostras
    ALLOCATE(amostras(numAmostras))
    
    ! Solicita as respostas para cada amostra
    DO i = 1, numAmostras
        WRITE(*,*) 'Insira a resposta para a amostra', i, ':'
        READ(*,*) resposta
        amostras(i) = resposta
    END DO
    
    ! Calcula a média das respostas
    soma = 0
    DO i = 1, numAmostras
        soma = soma + amostras(i)
    END DO
    media = soma / numAmostras
    
    ! Calcula a moda das respostas
    moda = amostras(1)
    DO i = 2, numAmostras
        INTEGER :: contador
        contador = 1
        DO j = i + 1, numAmostras
            IF (amostras(i) == amostras(j)) THEN
                contador = contador + 1
            END IF
        END DO
        IF (contador > moda) THEN
            moda = amostras(i)
        END IF
    END DO
    
    ! Calcula a mediana das respostas
    CALL bubbleSort(amostras)
    IF (MOD(numAmostras, 2) == 0) THEN
        mediana = (amostras(numAmostras/2) + amostras(numAmostras/2 + 1)) / 2
    ELSE
        mediana = amostras((numAmostras + 1) / 2)
    END IF
    
    ! Exibe os resultados
    WRITE(*,*) 'Média:', media
    WRITE(*,*) 'Moda:', moda
    WRITE(*,*) 'Mediana:', mediana
    
    ! Libera a memória alocada
    DEALLOCATE(amostras)
    
    STOP
    
CONTAINS

    SUBROUTINE bubbleSort(arr)
        INTEGER, DIMENSION(:), INTENT(INOUT) :: arr
        INTEGER :: i, j, temp
    
        DO i = 1, SIZE(arr) - 1
            DO j = 1, SIZE(arr) - i
                IF (arr(j) > arr(j + 1)) THEN
                    temp = arr(j)
                    arr(j) = arr(j + 1)
                    arr(j + 1) = temp
                END IF
            END DO
        END DO
    
    END SUBROUTINE bubbleSort
    
END PROGRAM AnaliseDeDados
```

Neste código, utilizei um algoritmo de ordenação conhecido como Bubble Sort para ordenar as amostras em ordem crescente antes de calcular a mediana. A função `bubbleSort` é responsável por realizar essa ordenação.